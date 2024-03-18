/** @type {import('tailwindcss').Config} */
module.exports = {
    mode: "jit",
    content: [
        "./index.html",
        "./**/*.{fs,js,ts,jsx,tsx}",
    ],
    theme: {
        extend: {typography: {
        '.preview': {
          css: {
            '*': {
              all: 'unset',
            },
          },
        },
      },},
    },
    plugins: []
}