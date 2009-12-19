#include <QStringList>
#include <QDebug>
#include <QXmlSimpleReader>

#include "mainwindow.h"
#include "context.h"
#include "contextreader.h"

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);

    QStringList args = app.arguments();
    if(args.contains("-c") || args.contains("--context"))
    {
        int i = args.indexOf(QString("-c"));
        if(i == -1)
            i = args.indexOf(QString("--context"));
        if(args.count() <= (i+1))
        {
            qDebug() << "Context file not specified.";
            return -1;
        }
        QFile contextFile(args.at(i+1));
        QXmlSimpleReader reader;
        ContextReader handler;
        reader.setContentHandler(&handler);
        reader.setErrorHandler(&handler);
        QXmlInputSource xmlInput(&contextFile);
        if(!reader.parse(&contextFile))
        {
            qDebug() << "Error parsing context file.";
            return -1;
        }
    }
    else
    {
        qDebug() << "Context file not specified.";
        return -1;
    }

    MainWindow *mainwindow = new MainWindow();
    mainwindow->show();
    return app.exec();
}
