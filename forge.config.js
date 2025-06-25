module.exports = {
  packagerConfig: (arch, buildPlatform) => {
    const commonConfig = {
      executableName: 'ShinyDeer',
      icon: './icon', // icon.ico for Windows, icon.icns for macOS
    };

    let extraResources = [];

    if (buildPlatform === 'darwin') {
      extraResources = ['r-mac/fontconfig/fonts/conf.d'];
    } else if (buildPlatform === 'win32') {
      extraResources = ['r-win/'];
    }

    return {
      ...commonConfig,
      extraResources,
    };
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