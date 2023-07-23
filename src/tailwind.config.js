/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["../public/**/*.html", "../app/**/*.hs"],
  theme: {
    container: {
      center: true,
    },
    extend: {},
  },
  plugins: [require("@tailwindcss/typography"), require("daisyui")],
};
