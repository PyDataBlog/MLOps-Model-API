import { editMenu, viewMenu, windowMenu, helpMenu } from './common-menus'
import { addDarwinMenuItems } from './darwin-menus'
import { app, Menu } from 'electron'

const initialMenu = [ editMenu, viewMenu, windowMenu, helpMenu ]

export const setupMenus = () => {
  const menuItems = (process.platform === 'darwin')
    ? addDarwinMenuItems(initialMenu)
    : initialMenu

  const menu = Menu.buildFromTemplate(menuItems)
  Menu.setApplicationMenu(menu)
}
