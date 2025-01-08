FROM nixos/nix:latest AS builder

RUN echo "experimental-features = nix-command flakes" >> /etc/nix/nix.conf

WORKDIR /app
COPY flake.nix flake.lock ./
COPY . .

RUN nix develop -c bash -c "\
    dotnet tool restore && \
    dotnet paket restore && \
    dotnet run Bundle && \
    cd Documentation && mkdocs build"

FROM nginx:1.25.1-alpine

COPY <<'EOF' /etc/nginx/nginx.conf
events {
    worker_connections 1024;
}
http {
    include /etc/nginx/mime.types;
    default_type application/octet-stream;

    access_log /var/log/nginx/access.log;
    error_log /var/log/nginx/error.log;

    server {
        listen 8080;
        server_name localhost;

        location / {
            root /usr/share/nginx/html/app;
            index index.html;
            try_files $uri $uri/ /index.html;
        }
    }

    server {
        listen 8082;
        server_name localhost;

        location / {
            root /usr/share/nginx/html/docs;
            index index.html;
            try_files $uri $uri/ /index.html;
        }
    }
}
EOF

COPY --from=builder /app/deploy/public/ /usr/share/nginx/html/app/
COPY --from=builder /app/Documentation/site/ /usr/share/nginx/html/docs/

EXPOSE 8080 8082
CMD ["nginx", "-g", "daemon off;"]