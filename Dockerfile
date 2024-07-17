FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build

# Install node
ARG NODE_MAJOR=20
RUN apt-get update
RUN apt-get install -y ca-certificates curl gnupg
RUN mkdir -p /etc/apt/keyrings
RUN curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg
RUN echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_$NODE_MAJOR.x nodistro main" | tee /etc/apt/sources.list.d/nodesource.list
RUN apt-get update && apt-get install nodejs -y


# Set the working directory
WORKDIR /app

# Copy the project files
COPY . .

# Restore .NET tools and dependencies
RUN dotnet tool restore
RUN dotnet paket restore

# Build the application
RUN dotnet run Bundle

# Use a specific version of the Nginx image for the final stage
FROM nginx:1.25.1-alpine

# Copy the bundled application from the build stage
COPY --from=build /app/deploy/public/ /usr/share/nginx/html

# Expose port 80
EXPOSE 80

# Start Nginx server
CMD ["nginx", "-g", "daemon off;"]
