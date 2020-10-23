module.exports = {
  purge: false,
  theme: {
    inset: {
      '0': 0,
      auto: 'auto',
      '1/2': '50%',
    }
  },
  variants: {
      display: ['responsive', 'hover', 'focus', 'group-hover'],
      visibility: ['responsive', 'hover', 'focus', 'group-hover'],
      opacity: ['responsive', 'hover', 'focus', 'group-hover'],
  },
  plugins: [],
};
