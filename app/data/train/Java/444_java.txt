package model.question;

import model.enums.QuestionTypes;

public class ImageQuestion implements QuestionIF {
	
	private String imagePath;
	private char[] answer;
	private QuestionTypes type;
	
	public ImageQuestion(String imagePath, char[] answer, QuestionTypes type){
		this.setType(type);
		this.setImagePath(imagePath);
		this.setAnswer(answer);
	}
	
	private void setType(QuestionTypes type) {
		// TODO Auto-generated method stub
		this.type = type;
		
	}

	private void setAnswer(char[] answer) {
		// TODO Auto-generated method stub
		this.answer = answer;
	}

	private void setImagePath(String imagePath) {
		// TODO Auto-generated method stub
		this.imagePath = imagePath;
	}

	// ************************************************************************
	//				 	get question from file
	// ************************************************************************
	@Override
	public String getQuestion() {
		// TODO Auto-generated method stub
		return imagePath;
	}
	
	// ************************************************************************
	//				 get the answer read from file
	// ************************************************************************
	@Override
	public char[] getAnswer() {
		// TODO Auto-generated method stub
		return answer;
	}

	@Override
	public QuestionTypes getType() {
		// TODO Auto-generated method stub
		return type;
	}

}
