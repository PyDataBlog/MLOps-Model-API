// Mainframe macro generated from application: /opt/local/libexec/root5/bin/root.exe
// By ROOT version 5.34/28 on 2015-04-14 00:54:47

#ifndef ROOT_TGDockableFrame
#include "TGDockableFrame.h"
#endif
#ifndef ROOT_TGMenu
#include "TGMenu.h"
#endif
#ifndef ROOT_TGMdiDecorFrame
#include "TGMdiDecorFrame.h"
#endif
#ifndef ROOT_TG3DLine
#include "TG3DLine.h"
#endif
#ifndef ROOT_TGMdiFrame
#include "TGMdiFrame.h"
#endif
#ifndef ROOT_TGMdiMainFrame
#include "TGMdiMainFrame.h"
#endif
#ifndef ROOT_TGMdiMenu
#include "TGMdiMenu.h"
#endif
#ifndef ROOT_TGListBox
#include "TGListBox.h"
#endif
#ifndef ROOT_TGNumberEntry
#include "TGNumberEntry.h"
#endif
#ifndef ROOT_TGScrollBar
#include "TGScrollBar.h"
#endif
#ifndef ROOT_TGComboBox
#include "TGComboBox.h"
#endif
#ifndef ROOT_TGuiBldHintsEditor
#include "TGuiBldHintsEditor.h"
#endif
#ifndef ROOT_TRootBrowser
#include "TRootBrowser.h"
#endif
#ifndef ROOT_TGuiBldNameFrame
#include "TGuiBldNameFrame.h"
#endif
#ifndef ROOT_TGFrame
#include "TGFrame.h"
#endif
#ifndef ROOT_TGFileDialog
#include "TGFileDialog.h"
#endif
#ifndef ROOT_TGShutter
#include "TGShutter.h"
#endif
#ifndef ROOT_TGButtonGroup
#include "TGButtonGroup.h"
#endif
#ifndef ROOT_TGCommandPlugin
#include "TGCommandPlugin.h"
#endif
#ifndef ROOT_TGCanvas
#include "TGCanvas.h"
#endif
#ifndef ROOT_TGFSContainer
#include "TGFSContainer.h"
#endif
#ifndef ROOT_TGuiBldEditor
#include "TGuiBldEditor.h"
#endif
#ifndef ROOT_TGColorSelect
#include "TGColorSelect.h"
#endif
#ifndef ROOT_TGTextEdit
#include "TGTextEdit.h"
#endif
#ifndef ROOT_TGButton
#include "TGButton.h"
#endif
#ifndef ROOT_TGFSComboBox
#include "TGFSComboBox.h"
#endif
#ifndef ROOT_TGLabel
#include "TGLabel.h"
#endif
#ifndef ROOT_TGView
#include "TGView.h"
#endif
#ifndef ROOT_TGMsgBox
#include "TGMsgBox.h"
#endif
#ifndef ROOT_TRootGuiBuilder
#include "TRootGuiBuilder.h"
#endif
#ifndef ROOT_TGFileBrowser
#include "TGFileBrowser.h"
#endif
#ifndef ROOT_TGTab
#include "TGTab.h"
#endif
#ifndef ROOT_TGListView
#include "TGListView.h"
#endif
#ifndef ROOT_TGSplitter
#include "TGSplitter.h"
#endif
#ifndef ROOT_TGTextEditor
#include "TGTextEditor.h"
#endif
#ifndef ROOT_TRootCanvas
#include "TRootCanvas.h"
#endif
#ifndef ROOT_TGStatusBar
#include "TGStatusBar.h"
#endif
#ifndef ROOT_TGListTree
#include "TGListTree.h"
#endif
#ifndef ROOT_TGuiBldGeometryFrame
#include "TGuiBldGeometryFrame.h"
#endif
#ifndef ROOT_TGToolTip
#include "TGToolTip.h"
#endif
#ifndef ROOT_TGToolBar
#include "TGToolBar.h"
#endif
#ifndef ROOT_TRootEmbeddedCanvas
#include "TRootEmbeddedCanvas.h"
#endif
#ifndef ROOT_TCanvas
#include "TCanvas.h"
#endif
#ifndef ROOT_TGuiBldDragManager
#include "TGuiBldDragManager.h"
#endif
#ifndef ROOT_TGHtmlBrowser
#include "TGHtmlBrowser.h"
#endif

#include "Riostream.h"

void gui4()
{

   // main frame
   TGMainFrame *fMainFrame2948 = new TGMainFrame(gClient->GetRoot(),10,10,kMainFrame | kVerticalFrame);
   fMainFrame2948->SetName("fMainFrame2948");
   fMainFrame2948->SetLayoutBroken(kTRUE);

   // composite frame
   TGCompositeFrame *Ratioizer = new TGCompositeFrame(fMainFrame2948,848,609,kVerticalFrame);
   Ratioizer->SetName("Ratioizer");
   Ratioizer->SetLayoutBroken(kTRUE);

   // composite frame
   TGCompositeFrame *fMainFrame1943 = new TGCompositeFrame(Ratioizer,490,372,kVerticalFrame);
   fMainFrame1943->SetName("fMainFrame1943");

   // horizontal frame
   TGHorizontalFrame *fHorizontalFrame1284 = new TGHorizontalFrame(fMainFrame1943,488,370,kHorizontalFrame);
   fHorizontalFrame1284->SetName("fHorizontalFrame1284");

   // canvas widget
   TGCanvas *fCanvas1303 = new TGCanvas(fHorizontalFrame1284,102,366);
   fCanvas1303->SetName("fCanvas1303");

   // canvas viewport
   TGViewPort *fViewPort2150 = fCanvas1303->GetViewPort();

   // list tree
   TGListTree *fListTree1313 = new TGListTree(fCanvas1303,kHorizontalFrame);
   fListTree1313->SetName("fListTree1313");

   const TGPicture *popen;       //used for list tree items
   const TGPicture *pclose;      //used for list tree items

   TGListTreeItem *item10 = fListTree1313->AddItem(NULL,"Entry 1");
   item10->SetPictures(popen, pclose);
   fListTree1313->CloseItem(item10);
   TGListTreeItem *item11 = fListTree1313->AddItem(NULL,"Entry 2");
   item11->SetPictures(popen, pclose);
   fListTree1313->CloseItem(item11);
   fListTree1313->SetSelected(item11);
   TGListTreeItem *item12 = fListTree1313->AddItem(NULL,"Entry 3");
   item12->SetPictures(popen, pclose);
   fListTree1313->CloseItem(item12);
   TGListTreeItem *item13 = fListTree1313->AddItem(NULL,"Entry 4");
   item13->SetPictures(popen, pclose);
   fListTree1313->CloseItem(item13);
   TGListTreeItem *item14 = fListTree1313->AddItem(NULL,"Entry 5");
   item14->SetPictures(popen, pclose);
   fListTree1313->CloseItem(item14);

   fViewPort2150->AddFrame(fListTree1313);
   fListTree1313->SetLayoutManager(new TGHorizontalLayout(fListTree1313));
   fListTree1313->MapSubwindows();
   fCanvas1303->SetContainer(fListTree1313);
   fCanvas1303->MapSubwindows();
   fHorizontalFrame1284->AddFrame(fCanvas1303, new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsExpandY,2,2,2,2));

   // vertical frame
   TGVerticalFrame *fVerticalFrame1356 = new TGVerticalFrame(fHorizontalFrame1284,378,366,kVerticalFrame);
   fVerticalFrame1356->SetName("fVerticalFrame1356");

   // horizontal frame
   TGHorizontalFrame *fHorizontalFrame1392 = new TGHorizontalFrame(fVerticalFrame1356,333,140,kHorizontalFrame);
   fHorizontalFrame1392->SetName("fHorizontalFrame1392");

   // "Numerator" group frame
   TGGroupFrame *numerator_frame = new TGGroupFrame(fHorizontalFrame1392,"Numerator");
   numerator_frame->SetTitlePos(TGGroupFrame::kCenter);

   // embedded canvas
   TRootEmbeddedCanvas *fRootEmbeddedCanvas2163 = new TRootEmbeddedCanvas(0,numerator_frame,102,102);
   Int_t wfRootEmbeddedCanvas2163 = fRootEmbeddedCanvas2163->GetCanvasWindowId();
   TCanvas *c129 = new TCanvas("c129", 10, 10, wfRootEmbeddedCanvas2163);
   fRootEmbeddedCanvas2163->AdoptCanvas(c129);
   numerator_frame->AddFrame(fRootEmbeddedCanvas2163, new TGLayoutHints(kLHintsLeft | kLHintsCenterX | kLHintsTop | kLHintsCenterY,2,2,2,2));

   numerator_frame->SetLayoutManager(new TGMatrixLayout(numerator_frame,1,1,7,0));
   numerator_frame->Resize(116,131);
   fHorizontalFrame1392->AddFrame(numerator_frame, new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsExpandX | kLHintsExpandY,2,20,2,2));

   // "Denominator" group frame
   TGGroupFrame *denominator_frame = new TGGroupFrame(fHorizontalFrame1392,"Denominator");
   denominator_frame->SetTitlePos(TGGroupFrame::kCenter);

   // embedded canvas
   TRootEmbeddedCanvas *fRootEmbeddedCanvas2177 = new TRootEmbeddedCanvas(0,denominator_frame,102,102);
   Int_t wfRootEmbeddedCanvas2177 = fRootEmbeddedCanvas2177->GetCanvasWindowId();
   TCanvas *c130 = new TCanvas("c130", 10, 10, wfRootEmbeddedCanvas2177);
   fRootEmbeddedCanvas2177->AdoptCanvas(c130);
   denominator_frame->AddFrame(fRootEmbeddedCanvas2177, new TGLayoutHints(kLHintsLeft | kLHintsCenterX | kLHintsTop | kLHintsCenterY,2,2,2,2));

   denominator_frame->SetLayoutManager(new TGVerticalLayout(denominator_frame));
   denominator_frame->Resize(116,131);
   fHorizontalFrame1392->AddFrame(denominator_frame, new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsExpandX | kLHintsExpandY,20,2,2,2));

   fVerticalFrame1356->AddFrame(fHorizontalFrame1392, new TGLayoutHints(kLHintsLeft | kLHintsCenterX | kLHintsTop,9,2,2,2));

   // "Ratio" group frame
   TGGroupFrame *ratio_frame = new TGGroupFrame(fVerticalFrame1356,"Ratio");
   ratio_frame->SetTitlePos(TGGroupFrame::kCenter);

   // embedded canvas
   TRootEmbeddedCanvas *fRootEmbeddedCanvas2207 = new TRootEmbeddedCanvas(0,ratio_frame,102,102);
   Int_t wfRootEmbeddedCanvas2207 = fRootEmbeddedCanvas2207->GetCanvasWindowId();
   TCanvas *c131 = new TCanvas("c131", 10, 10, wfRootEmbeddedCanvas2207);
   fRootEmbeddedCanvas2207->AdoptCanvas(c131);
   ratio_frame->AddFrame(fRootEmbeddedCanvas2207, new TGLayoutHints(kLHintsLeft | kLHintsTop,2,9,2,2));

   ratio_frame->SetLayoutManager(new TGVerticalLayout(ratio_frame));
   ratio_frame->Resize(143,136);
   fVerticalFrame1356->AddFrame(ratio_frame, new TGLayoutHints(kLHintsLeft | kLHintsCenterX | kLHintsTop,2,2,2,2));

   fHorizontalFrame1284->AddFrame(fVerticalFrame1356, new TGLayoutHints(kLHintsLeft | kLHintsTop | kLHintsExpandX | kLHintsExpandY,2,2,2,2));

   fMainFrame1943->AddFrame(fHorizontalFrame1284, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY,1,1,1,1));

   Ratioizer->AddFrame(fMainFrame1943, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
   fMainFrame1943->MoveResize(0,0,490,372);

   fMainFrame2948->AddFrame(Ratioizer, new TGLayoutHints(kLHintsExpandX | kLHintsExpandY));
   Ratioizer->MoveResize(0,0,848,609);

   fMainFrame2948->SetMWMHints(kMWMDecorAll,
                        kMWMFuncAll,
                        kMWMInputModeless);
   fMainFrame2948->MapSubwindows();

   fMainFrame2948->Resize(fMainFrame2948->GetDefaultSize());
   fMainFrame2948->MapWindow();
   fMainFrame2948->Resize(490,372);
}
