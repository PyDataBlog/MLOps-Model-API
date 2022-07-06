#include "bombyx.h"
#include "ui_bombyx.h"

Bombyx::Bombyx(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::Bombyx)
{
    ui->setupUi(this);
    QObject::connect(ui->but_draw, SIGNAL(clicked()), this, SLOT(graph()));
    QObject::connect(ui->but_draw2, SIGNAL(clicked()), this, SLOT(graph2()));
}

void Bombyx::graph()
{
    QVector <double> x(100), y(100);

    k = ui->spb_k->value();
    y[0] = 10;
    x[0] = 1;

    for (int i=1; i<100; ++i)
    {
        x[i] = i + 1;
        y[i] = k * y[i - 1] * ((1000 - y[i - 1]) / 1000);
    }
    ui->plot->addGraph();
    ui->plot->graph(0)->setPen(QPen(Qt::red));
    ui->plot->graph(0)->setData(x, y);
    ui->plot->graph(0)->rescaleAxes(true);
    ui->plot->setInteractions(QCP::iRangeDrag | QCP::iRangeZoom | QCP::iSelectPlottables);
    ui->plot->replot();
}

void Bombyx::graph2()
{
    double prec;
    QVector <double> x, y;

    imin = ui->spb_imin->value();
    imax = ui->spb_imax->value();
    for (double i = 1.0; i < 4.0; i += 0.01)
    {
        prec = 10;
        for (int j = 1; j <= imax; ++j)
        {
            if (j >= imin && j <= imax)
            {
                x.append(i);
                y.append(i * prec * ((1000 - prec) / 1000));
            }
            prec = i * prec * ((1000 - prec) / 1000);
        }
    }
    ui->plot2->addGraph();
    ui->plot2->graph(0)->setPen(QPen(Qt::blue));
    ui->plot2->graph(0)->setScatterStyle(QCPScatterStyle::ssPlus);
    ui->plot2->graph(0)->setLineStyle(QCPGraph::lsNone);
    ui->plot2->graph(0)->setData(x, y);
    ui->plot2->graph(0)->rescaleAxes(true);
    ui->plot2->setInteractions(QCP::iRangeDrag | QCP::iRangeZoom | QCP::iSelectPlottables);
    ui->plot2->replot();
}

Bombyx::~Bombyx()
{
    delete ui;
}
