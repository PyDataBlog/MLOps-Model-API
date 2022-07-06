"""
    @author: ArcGIS for Intelligence
    @contact: defensesolutions@esri.com
    @company: Esri
    @version: 1.0
    @description: Used to stage the apps for Movement Analysis
    @requirements: Python 2.7.x, ArcGIS 10.3.1
    @copyright: Esri, 2015
"""
import arcresthelper
from arcresthelper import portalautomation

log_file='./logs/DamageAssessment.log'
configFiles=  ['./configs/StageApp.json']
globalLoginInfo = './configs/GlobalLoginInfo.json'
dateTimeFormat = '%Y-%m-%d %H:%M'

pa = portalautomation.portalautomation(globalLoginInfo)
pa.setLog(log_file=log_file)
pa.publishfromconfig(configFiles=configFiles, 
                        combinedApp=None,
                        dateTimeFormat=dateTimeFormat)
del pa