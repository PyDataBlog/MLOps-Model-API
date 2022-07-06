/********************************************************************************
*					Tangram Library - version 8.0								*
*********************************************************************************
* Copyright (C) 2002-2015 by Tangram Team.   All Rights Reserved.				*
*
* THIS SOURCE FILE IS THE PROPERTY OF TANGRAM TEAM AND IS NOT TO
* BE RE-DISTRIBUTED BY ANY MEANS WHATSOEVER WITHOUT THE EXPRESSED
* WRITTEN CONSENT OF TANGRAM TEAM.
*
* THIS SOURCE CODE CAN ONLY BE USED UNDER THE TERMS AND CONDITIONS
* OUTLINED IN THE GPL LICENSE AGREEMENT.TANGRAM TEAM
* GRANTS TO YOU (ONE SOFTWARE DEVELOPER) THE LIMITED RIGHT TO USE
* THIS SOFTWARE ON A SINGLE COMPUTER.
*
* CONTACT INFORMATION:
* mailto:sunhui@tangramfx.com
* http://www.tangramFX.com
*
*
********************************************************************************/

#include "stdafx.h"
#include "CommonIncludes.h"
#include "TangramToolWindow.h"
#include "TangramVSIApp.h"

const GUID& CTangramToolWnd::GetToolWindowGuid() const
{
	return CLSID_guidPersistanceSlot;
}

void CTangramToolWnd::PostCreate()
{
	CComVariant srpvt;
	srpvt.vt = VT_I4;
	srpvt.intVal = IDB_IMAGES;
	// We don't want to make the window creation fail only becuase we can not set
	// the icon, so we will not throw if SetProperty fails.
	if (SUCCEEDED(GetIVsWindowFrame()->SetProperty(VSFPROPID_BitmapResource, srpvt)))
	{
		srpvt.intVal = 1;
		GetIVsWindowFrame()->SetProperty(VSFPROPID_BitmapIndex, srpvt);
	}

	if (theApp.m_pTangramCore&&m_pPane)
	{
		HWND h = m_pPane->m_hWnd;

		//Begin add By Tangram Team: Step 2
		theApp.m_pTangramCore->put_ToolWndHandle((LONGLONG)(h));
		theApp.m_hVSToolWnd = h;
		theApp.m_pTangramCore->CreateTangram((LONGLONG)::GetParent(h), &m_pTangram);
		if (m_pTangram)
		{
			CComBSTR strAppStartFilePath(L"res://StartupPage.dll/start.htm");
			//CComBSTR strAppStartFilePath(L"http://tangramcloud.com/static/startup.html");
			m_pTangram->put_URL(strAppStartFilePath);
		}
	}
	//End add By Tangram Team!

}
