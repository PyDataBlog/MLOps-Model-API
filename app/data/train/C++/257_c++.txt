//----------------------------------------------------------------------------

#pragma hdrstop
#include <stdio.h>
#include <memory>

#include "uCM.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "Vcl.Controls.TControl"
#pragma resource "*.dfm"
TCM *CM;
//---------------------------------------------------------------------------
__fastcall TCM::TCM(TComponent* Owner)
	: TDataModule(Owner)
{
	FInstanceOwner = true;
}

__fastcall TCM::~TCM()
{
	delete FSMClient;
}

TSMClient* TCM::GetSMClient(void)
{
	if (FSMClient == NULL)
		FSMClient= new TSMClient(DSRestConnection1, FInstanceOwner);
	return FSMClient;
};

