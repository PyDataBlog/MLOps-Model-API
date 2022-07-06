/*
 * File:   frmHelp.cpp
 * Author: matej
 *
 * Created on July 24, 2011, 8:33 PM
 */

#include "frmHelp.h"
#include <QFile>

frmHelp::frmHelp()
{
    widget.setupUi(this);
}

frmHelp::~frmHelp()
{
}

void frmHelp::loadHelpFile( QString helpFile )
{
    QFile file( helpFile );
    if( !file.open( QIODevice::ReadOnly | QIODevice::Text ) )
    {
        widget.textHelp->setText( "Sorry, help file not found." );
    }
    widget.textHelp->setHtml( file.readAll() );
}
