package models

type SurveyResult struct {
	SurveyId   int
	Answers    []Answer
	DeviceInfo DeviceInfomation
}

type Question struct {
	QuestionId int
	Type       int //0代表单选题，1代表多选题，2代表填空题
	Title      string
	Content    []string
	// CreatedTime int64
}

type Survey struct {
	SurveyId    int
	Title       string
	Description string
	// CreatedTime int64
}

type Answer struct {
	QuestionId   int
	AnswerId     int
	QuestionType int
	Options      []int
	Fill         string
	CreatedTime  int64
}

type DeviceInfomation struct {
	DeviceId      int
	DeviceName    string
	SystemVersion string
	AppVersion    string
}
