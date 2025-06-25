const os = require('os');

module.exports = {
  packagerConfig: {
    executableName: 'ShinyDeer',
    extraResources: os.platform() === 'darwin' ? [
      'r-mac/fontconfig/fonts/conf.d'
    ] : [],
  },
  makers: [
    {
      name: '@electron-forge/maker-deb',
      config: {},
    },
  ],
};
