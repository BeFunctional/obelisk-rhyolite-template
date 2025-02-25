const colors = require("tailwindcss/colors");


module.exports = {
  // Enable on deploy only! see: https://tailwindcss.com/docs/optimizing-for-production
  purge: {
    content: ["./frontend/**/*.hs"],
  },
  darkMode: 'media', // or 'media' or 'class'
  // theme: {
  // },
  plugins: ["@tailwindcss/forms"],
  layers: {
    components: {
      '.kepler-gl': {
        height: '100% !important',
        width: '100% !important'
      }
    }
  },
  variants: {
    extend: {
      visibility: ["group-hover"],
    },
  },
};
