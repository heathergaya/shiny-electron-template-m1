const os = require('os');

module.exports = {
  packagerConfig: {
    executableName: 'ShinyDeer',
    icon: './icon', // Windows expects .ico, macOS uses .icns automatically
    extraResources: os.platform() === 'darwin'
      ? ['r-mac/fontconfig/fonts/conf.d']
      : [],
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