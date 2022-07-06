# sudo docker build -t="viatcheslavmogilevsky/gp_admin_base0.0.1-ruby2.1.2" .

FROM viatcheslavmogilevsky/gp_admin_base0.0.1
MAINTAINER V. Mogilevksy "https://github.com/viatcheslavmogilevsky"
ADD . /play
RUN ansible-playbook /play/setup.yml -c local