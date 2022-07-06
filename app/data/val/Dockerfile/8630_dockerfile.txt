FROM drydock/u14jav:{{%TAG%}}

ADD . /u14sca

RUN /u14sca/install.sh
