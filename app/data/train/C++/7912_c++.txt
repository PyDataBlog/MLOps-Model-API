#include <QApplication>
#include <QWebEngineView>
#include "Page.h"

int main(int argc, char **argv) {
	QApplication app(argc, argv);
	QWebEngineView *l=new QWebEngineView(0);
	Page *p=new Page(l);
	l->setPage(p);
	p->open("index.sh.htm");
	l->show();
	app.exec();
	delete l;
}
