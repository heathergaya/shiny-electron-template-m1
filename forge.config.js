const os = require('os');

let extraResources = [];
const platform = process.platform;

if (platform === 'darwin') {
  extraResources = ['r-mac/fontconfig/fonts/conf.d'];
} else if (platform === 'win32') {
  extraResources = ['r-win'];
}

module.exports = {
  packagerConfig: {
    executableName: 'ShinyDeer',
    icon: './icon',
    extraResources
  },
  makers: [
    {
      name: '@electron-forge/maker-deb',
      config: {}
    },
    {
      name: '@electron-forge/maker-zip',
      platforms: ['darwin'],
      config: {}
    },
    {
      name: '@electron-forge/maker-squirrel',
      config: {
        setupExe: 'ShinyDeerInstaller.exe',
        setupIcon: './icon.ico',
        shortcutName: 'ShinyDeer',
        runAfterFinish: true
      }
    }
  ]
};
