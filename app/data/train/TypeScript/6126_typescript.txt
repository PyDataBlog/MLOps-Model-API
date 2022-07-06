import * as plugins from './tools.plugins';
import * as toolsInstall from './tools.install';

export const run = async () => {
  const toolsCli = new plugins.smartcli.Smartcli();

  toolsCli.addCommand('install').subscribe(async (argvArg) => {
    toolsInstall.install('default');
  });

  toolsCli.addVersion('no version set');
  toolsCli.startParse();
};
