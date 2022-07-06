using namespace std;

struct _Course{
    string CourseCode;
    int Point;
    int Score;

    _Course(string CourseCode){
        this->CourseCode = CourseCode;
        this->Score = 0;
        this->Point = 0;
    }

    void setCourse(string CourseCode, int Score, int Point){
        this->CourseCode = CourseCode;
        this->Score = Score;
        this->Point = Point;
    }

    string getCode(){
        return this->CourseCode;
    }

    int getPoint(){
        return this->Point;
    }

    int getScore(){
        return this->Score;
    }
};

struct Student{
    bool ActFlag;
    string StudentID;
    string StudentName;
    int TotCoursePoints;
    int TotCredits;
    double Average;
    list<_Course> Course;

    Student(){
        this->ActFlag = true;
        this->StudentID = "D9999999";
        this->StudentName = "xxxxxxxxxxxxxxxxxxxx";
        this->TotCoursePoints = 0;
        this->TotCredits = 0;
        this->Average = 0.00;
    }

    void setIDandName(string ID, string Name){
        this->StudentID = ID;
        this->StudentName = Name;
        this->ActFlag = true;
    }

    bool getActFlag(){
        return this->ActFlag;
    }

    string getUID(){
        return this->StudentID;
    }

    string getName(){
        return this->StudentName;
    }

    int getTotCoursePoints(){
        return this->TotCoursePoints;
    }

    int getTotCredits(){
        return this->TotCredits;
    }

    double getAverage(){
        return this->Average;
    }

    void setAvgData(int TotCoursePoints, int TotCredits){
        this->TotCoursePoints = TotCoursePoints;
        this->TotCredits = TotCredits;
        if(TotCredits==0)
            this->Average = 0;
        else
            this->Average = (double)TotCoursePoints / (double)TotCredits;
    }

    void suspend(){
        this->ActFlag = false;
        this->TotCoursePoints = 0;
        this->TotCredits = 0;
        this->Average = 0.0;
        this->Course.clear();
    }
};
