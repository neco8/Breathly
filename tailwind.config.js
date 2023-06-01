/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{elm,ts}", "./styles/**/*.{scss,css}", "./index.html"],
  theme: {
    extend: {
      fontFamily: {
        "source-serif": "Source Serif Pro",
        "bodoni-moda": "Bodoni Moda",
        "noto-serif": "Noto Serif JP",
        main: "Noto Serif JP",
      },
    },
  },
  plugins: [],
};
