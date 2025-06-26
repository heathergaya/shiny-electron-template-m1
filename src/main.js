// Copyright (c) 2018 Dirk Schumacher, Noam Ross, Rich FitzJohn
// Copyright (c) 2024 Jinhwan Kim

import { app, session, BrowserWindow } from 'electron';
import path from 'path';
import http from 'axios';
import os from 'os';
import execa from 'execa';

// --- Setup ---
const rPath = os.platform() === 'win32' ? 'r-win' : 'r-mac';
const rpath = path.join(app.getAppPath(), rPath);
const libPath = path.join(rpath, 'library');
const rscript = os.platform() === 'win32'
  ? path.join(rpath, 'bin', 'R.exe')
  : path.join(rpath, 'bin', 'R');
const shinyAppPath = path.join(app.getAppPath(), 'shiny');
const backgroundColor = '#2c3e50';

console.log('Platform:', os.platform());
console.log('App path:', app.getAppPath());
console.log('Rscript path:', rscript);
console.log('Shiny app path:', shinyAppPath);

// --- Utility ---
const waitFor = ms => new Promise(resolve => setTimeout(resolve, ms));
const randomPort = exclude => {
  let min = 3000, max = 8000;
  let port;
  do {
    port = Math.floor(Math.random() * (max - min + 1)) + min;
  } while (exclude.includes(port));
  return port;
};

const shinyPort = randomPort([3659, 4045, 5060, 5061, 6000, 6566, 6665, 6666, 6667, 6668, 6669, 6697]);

// --- Globals ---
let shutdown = false;
let rShinyProcess = null;
let mainWindow;
let loadingSplashScreen;
let errorSplashScreen;

// --- Web Server Starter ---
const tryStartWebserver = async (attempt, progressCallback, onErrorStartup, onErrorLater, onSuccess) => {
  if (attempt > 100) {
    await progressCallback({ attempt, code: 'failed' });
    await onErrorStartup();
    return;
  }

  if (rShinyProcess !== null) {
    await onErrorStartup(); // already running
    return;
  }

  await progressCallback({ attempt, code: 'start' });
  console.log(`Attempt ${attempt}: Starting R process...`);

  let shinyRunning = false;
  let shinyProcessAlreadyDead = false;

  try {
    rShinyProcess = execa(rscript, ['--vanilla', '-f', path.join(app.getAppPath(), 'start-shiny.R')], {
      env: {
        WITHIN_ELECTRON: '1',
        RHOME: rpath,
        R_HOME_DIR: rpath,
        RE_SHINY_PORT: shinyPort,
        RE_SHINY_PATH: shinyAppPath,
        R_LIBS: libPath,
        R_LIBS_USER: libPath,
        R_LIBS_SITE: libPath,
        R_LIB_PATHS: libPath
      }
    });

    // Pipe logs to console
    rShinyProcess.stdout?.on('data', data => console.log('[R stdout]', data.toString()));
    rShinyProcess.stderr?.on('data', data => console.error('[R stderr]', data.toString()));

    const url = `http://127.0.0.1:${shinyPort}`;
    for (let i = 0; i < 50; i++) {
      if (shinyProcessAlreadyDead) break;
      await waitFor(500);
      try {
        const res = await http.head(url, { timeout: 1000 });
        if (res.status === 200) {
          shinyRunning = true;
          await progressCallback({ attempt, code: 'success' });
          console.log(`Shiny server is up at ${url}`);
          onSuccess(url);
          return;
        }
      } catch (e) { }
    }

    await progressCallback({ attempt, code: 'notresponding' });
    try { rShinyProcess.kill(); } catch (_) { }

  } catch (err) {
    shinyProcessAlreadyDead = true;
    console.error('Failed to start R:', err);
    await onErrorStartup();
  }
};

// --- Windows ---
const createWindow = (url) => {
  mainWindow = new BrowserWindow({
    width: 1600,
    height: 900,
    show: false,
    autoHideMenuBar: true,
    webPreferences: {
      nodeIntegration: false,
      contextIsolation: true
    }
  });

  console.log('Loading Shiny app at:', url);
  mainWindow.loadURL(url);
  mainWindow.once('ready-to-show', () => {
    mainWindow.show();
  });

  mainWindow.on('closed', () => {
    mainWindow = null;
  });
};

const splashScreenOptions = {
  width: 1600,
  height: 900,
  backgroundColor
};

const createSplashScreen = (filename) => {
  const splash = new BrowserWindow(splashScreenOptions);
  splash.loadURL(`file://${__dirname}/${filename}.html`);
  splash.on('closed', () => splash = null);
  return splash;
};

const createLoadingSplashScreen = () => {
  loadingSplashScreen = createSplashScreen('loading');
};

const createErrorScreen = () => {
  errorSplashScreen = createSplashScreen('failed');
};

// --- App Lifecycle ---
app.on('ready', async () => {
  session.defaultSession.webRequest.onHeadersReceived((_, callback) => {
    callback({
      responseHeaders: {
        "Content-Security-Policy": [
          "default-src 'none';",
          "script-src 'self';",
          "img-src 'self' data:;",
          "style-src 'self';",
          "font-src 'self';"
        ].join(' ')
      }
    });
  });

  session.defaultSession.setPermissionRequestHandler((_1, _2, callback) => callback(false));

  createLoadingSplashScreen();

  const emitSplashEvent = async (event, data) => {
    try {
      await loadingSplashScreen.webContents.send(event, data);
    } catch (e) {
      console.error('Splash screen event failed:', e);
    }
  };

  const progressCallback = async (event) => {
    await emitSplashEvent('start-webserver-event', event);
  };

  const onErrorLater = async () => {
    console.error('Shiny server crashed after startup');
    if (mainWindow) {
      createErrorScreen();
      await errorSplashScreen.show();
      mainWindow.destroy();
    }
  };

  const onErrorStartup = async () => {
    await waitFor(10000); // Wait for splash to load
    console.error('Shiny server failed to start');
    await emitSplashEvent('failed');
  };

  try {
    await tryStartWebserver(0, progressCallback, onErrorStartup, onErrorLater, (url) => {
      createWindow(url);
      loadingSplashScreen.destroy();
      loadingSplashScreen = null;
    });
  } catch (e) {
    console.error('Unexpected startup error:', e);
    await emitSplashEvent('failed');
  }
});

app.on('window-all-closed', () => {
  shutdown = true;
  if (rShinyProcess) {
    try { rShinyProcess.kill(); } catch (e) { }
  }
  app.quit();
});
