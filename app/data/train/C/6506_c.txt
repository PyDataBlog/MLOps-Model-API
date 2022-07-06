
class Singleton{
private:
  static Singleton *mInstance;//此处仅是申明
  Singleton();
  ~Singleton();

public:
  static Singleton* getInstance();
};
