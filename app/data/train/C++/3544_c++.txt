#include "meteodata.h"
#include "meteojour.h"
MeteoData::MeteoData(QString v,QObject *parent):QObject(parent),_ville(v){
    _mesure = "metric";
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : afficher message d'erreur dans la console
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::onError(){
    qDebug()<<"Erreur de requete";
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : effectuer une requete pour recupérer données JSON de l'API
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::requete(){
    QNetworkAccessManager *manager = new QNetworkAccessManager(this);
    qDebug()<<"Exe Req";
    //execution d'une requete
    //callback du storage a la reception de la requete
    qDebug()<<"Creation connexion pour succès ou echec";
    connect(manager,SIGNAL(finished(QNetworkReply*)),
            this,SLOT(storeReplyInObj(QNetworkReply*)));
    manager->get(QNetworkRequest(QUrl("http://api.openweathermap.org/data/2.5/forecast/daily?q="+_ville+"&appid=9a5b3401d0ae43c0fdd643de1a05660c&units="+_mesure+"&cnt=5")));
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : Stocker la réponse de l'API dans un QJsonObject
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::storeReplyInObj(QNetworkReply* r){
    qDebug()<<"CallBack";
    if(r->error() == QNetworkReply::NoError){
        QByteArray bts = r->readAll();
        QString str(bts);
        QJsonDocument doc = QJsonDocument::fromJson(str.toUtf8());
        if(!doc.isNull()){
            if(doc.isObject()){
                obj = doc.object();
                parseObj();
            }else{
                qDebug()<<"le doc n'est pas un objet";
            }
        } else {
            qDebug() << "JSON FORMAT INVALIDE";
        }
        //qDebug()<<bts;
    }else{
        qDebug()<<r->errorString();
    }
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : parser le QJsonDocument pour récupérer les données
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::parseObj(){
    qDebug() << obj.keys();//("city", "cnt", "cod", "list", "message")
    QJsonArray list = obj.value("list").toArray();
    for(int i = 0; i < 5;i ++){
        //qDebug() << list.at(i);
        QJsonObject jData = list.at(i).toObject();
        //qDebug() << jData.keys();//("clouds", "deg", "dt", "humidity", "pressure", "rain", "speed", "temp", "weather")
        QJsonObject jTemp = jData.value("temp").toObject();
        _coeffNuage = jData.value("clouds").toDouble();
        _coeffPluie = jData.value("rain").toDouble();
        _pressure = jData.value("pressure").toDouble();
        _humidity = jData.value("humidity").toDouble();
        _tempMin = jTemp.value("min").toDouble();
        _tempMax = jTemp.value("max").toDouble();
        _temp = jTemp.value("day").toDouble();
        emit dataChanged(i);
    }
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : getter humidité
 remarque:
 precond :
 postcond:
 ©2017
 */
double MeteoData::getHumidity()const{
    return _humidity;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : getter pression
 remarque:
 precond :
 postcond:
 ©2017
 */
double MeteoData::getPressure()const{
    return _pressure;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : getter temp min
 remarque:
 precond :
 postcond:
 ©2017
 */
double MeteoData::getTempMin()const{
    return _tempMin;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : getter temp max
 remarque:
 precond :
 postcond:
 ©2017
 */
double MeteoData::getTempMax()const{
    return _tempMax;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : getter temp actuelle
 remarque:
 precond :
 postcond:
 ©2017
 */
double MeteoData::getTemp()const{
    return _temp;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : setter humidité
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::setHumidity(double h){
  _humidity = h;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : setter pression
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::setPressure(double p){
  _pressure = p;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : setter temp min
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::setTempMin(double t){
  _tempMin = t;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : setter temp max
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::setTempMax(double t){
  _tempMax = t;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : setter temp actuelle
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::setTemp(double t){
  _temp = t;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : getter ville actuelle
 remarque:
 precond :
 postcond:
 ©2017
 */
QString MeteoData::getVille()const{
    return _ville;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : setter ville actuelle
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::setVille(QString v){
    _ville = v;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : getter coeff nuage
 remarque:
 precond :
 postcond:
 ©2017
 */
double MeteoData::getCoeffNuage()const{
    return _coeffNuage;

}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : getter coeff pluie
 remarque:
 precond :
 postcond:
 ©2017
 */
double MeteoData::getCoeffPluie()const{
    return _coeffPluie;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : setter coeff nuage
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::setCoeffNuage(double c){
    _coeffNuage = c;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : setter coeff pluie
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::setCoeffPluie(double c){
    _coeffPluie = c;
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : SLOT ré effectuer une requete après une action
 remarque:
 precond :
 postcond:
 ©2017
 */
void MeteoData::reqAgain(){
    requete();
}

/*
 author  : Fontaine pierre
 mail    : pierre.ftn64@gmail.com
 but     : setter mesure
 remarque: choix entre "metric,impérial,default"
 precond :
 postcond:
 ©2017
 */
void MeteoData::setMesure(QString s){
    _mesure = s;
}
