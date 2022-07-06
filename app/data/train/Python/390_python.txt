# -*- coding: utf-8 -*-
"""
Created on Tue May 28 12:20:59 2013

=== MAYAXES (v1.1) ===
Generates a set of MayaVI axes using the mayavi.mlab.axes() object with a 
white background, small black text and a centred title.  Designed to better 
mimic MATLAB style plots.  

Unspecified arguments will be set to default values when mayaxes is called 
(note that default settings are configured for a figure measuring 1024 x 768 
pixels and may not display correctly on plots that are significantly larger 
or smaller). 

=== Inputs ===
'title'         Figure title text (default = 'VOID') 
'xlabel'        X axis label text (default = 'X')
'ylabel'        Y axis label text (default = 'Y')
'zlabel'        Z axis label text (default = 'Z')
'handle'        Graphics handle of object (if bounding box is to be plotted)
'title_size'    Font size of the title text (default = 25)
'ticks'         Number of divisions on each axis (default = 7)
'font_scaling'  Font scaling factor for axis text (default = 0.7)
'background'    Background colour (can be 'b' (black) or 'w' (white))

=== Notes ===
Disbaling figure title: specify title_string='void' OR title_string='Void' OR 
title_string='VOID' to disable figure title.

Disabling bounding box: specify handle='void' OR handle='Void' OR handle='VOID' 
to disable figure bounding box.

=== Usage ===
from mayaxes import mayaxes
mayaxes('Figure title','X axis label','Y axis label','Z axis label')

OR

mayaxes(title_string='TITLE',xlabel='X',ylabel='Y',zlabel='Z',title_size=25,ticks=7,font_scaling=0.7)

=== Example ===
from mayaxes import test_mayaxes
test_mayaxes()

@author: Nathan Donaldson
"""

def mayaxes(title_string='VOID', xlabel='VOID', ylabel='VOID', zlabel='VOID', handle='VOID', \
    title_size=25, ticks=7, font_scaling=0.7, background='w'):
    
    if type(title_string) != str or type(xlabel) != str  or type(ylabel) != str or type(zlabel) != str:
        print('ERROR: label inputs must all be strings')  
        return
    elif type(ticks) != int:
        print('ERROR: number of ticks must be an integer')
        return
    elif type(font_scaling) != float and type(font_scaling) != int:
        print('Error: font scaling factor must be an integer or a float')
        return
    
    from mayavi.mlab import axes,title,gcf,outline
    
    # Create axes object
    ax = axes()
    
    # Font factor globally adjusts figure text size
    ax.axes.font_factor = font_scaling
    
    # Number of ticks along each axis
    ax.axes.number_of_labels = ticks
   
    # Set axis labels to input strings
    # (spaces are included for padding so that labels do not intersect with axes)
    if xlabel=='void' or xlabel=='Void' or xlabel=='VOID':
        print 'X axis label title disabled'
    else:
        ax.axes.x_label = '          ' + xlabel 

    if ylabel=='void' or ylabel=='Void' or ylabel=='VOID':
        print 'Y axis label disabled'
    else:
        ax.axes.y_label = ylabel + '          '

    if zlabel=='void' or zlabel=='Void' or zlabel=='VOID':
        print 'Z axis label disabled'
    else:
        ax.axes.z_label = zlabel + '     '
    
    # Create figure title
    if title_string=='void' or title_string=='Void' or title_string=='VOID':
        print 'Figure title disabled'
    else:
        text_title = title(title_string)
        text_title.x_position = 0.5
        text_title.y_position = 0.9
        text_title.property.color = (0.0, 0.0, 0.0)
        text_title.actor.text_scale_mode = 'none'
        text_title.property.font_size = title_size
        text_title.property.justification = 'centered'
        
    # Create bounding box
    if handle=='void' or handle=='Void' or handle=='VOID':
        print 'Bounding box disabled'
    else:
        if background == 'w':
            bounding_box = outline(handle, color=(0.0, 0.0, 0.0), opacity=0.2)
        elif background == 'b':
            bounding_box = outline(handle, color=(1.0, 1.0, 1.0), opacity=0.2)
        
    # Set axis, labels and titles to neat black text
    #ax.property.color = (0.0, 0.0, 0.0)
    #ax.title_text_property.color = (0.0, 0.0, 0.0)
    #ax.label_text_property.color = (0.0, 0.0, 0.0)
    ax.label_text_property.bold = False
    ax.label_text_property.italic = False
    ax.title_text_property.italic = False
    ax.title_text_property.bold = False

    # Reset axis range
    ax.axes.use_ranges = True

    # Set scene background, axis and text colours    
    fig = gcf()    
    if background == 'w':
        fig.scene.background = (1.0, 1.0, 1.0)
        ax.label_text_property.color = (0.0, 0.0, 0.0)
        ax.property.color = (0.0, 0.0, 0.0)
        ax.title_text_property.color = (0.0, 0.0, 0.0)
    elif background == 'b':
        fig.scene.background = (0.0, 0.0, 0.0)
        ax.label_text_property.color = (1.0, 1.0, 1.0)
        ax.property.color = (1.0, 1.0, 1.0)
        ax.title_text_property.color = (1.0, 1.0, 1.0)
    fig.scene.parallel_projection = True
    
def test_mayaxes():

    from mayaxes import mayaxes
    from scipy import sqrt,sin,meshgrid,linspace,pi
    import mayavi.mlab as mlab
        
    resolution = 200
    lambda_var = 3
    theta = linspace(-lambda_var*2*pi,lambda_var*2*pi,resolution)
    
    x, y = meshgrid(theta, theta)
    r = sqrt(x**2 + y**2)
    z = sin(r)/r
    
    fig = mlab.figure(size=(1024,768))
    surf = mlab.surf(theta,theta,z,colormap='jet',opacity=1.0,warp_scale='auto') 
    mayaxes(title_string='Figure 1: Diminishing polar cosine series', \
        xlabel='X data',ylabel='Y data',zlabel='Z data',handle=surf)
    
    fig.scene.camera.position = [435.4093863309094, 434.1268937227623, 315.90311468125287]
    fig.scene.camera.focal_point = [94.434632665253829, 93.152140057106593, -25.071638984402856]
    fig.scene.camera.view_angle = 30.0
    fig.scene.camera.view_up = [0.0, 0.0, 1.0]
    fig.scene.camera.clipping_range = [287.45231734040635, 973.59247058049255]
    fig.scene.camera.compute_view_plane_normal()
    fig.scene.render()   
    
    mlab.show() 
