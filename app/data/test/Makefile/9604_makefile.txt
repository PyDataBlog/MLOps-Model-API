all: grafik office video web edu audio games

grafik: grafik.tex
	pdflatex grafik.tex

office: office.tex
	pdflatex office.tex

video: video.tex
	pdflatex video.tex

web: web.tex
	pdflatex web.tex

edu: edu.tex
	pdflatex edu.tex

audio: audio.tex
	pdflatex audio.tex

games: games.tex
	pdflatex games.tex

clean:
	rm *.log *.aux *.out 
