FROM env/vim/js
MAINTAINER zachbray@googlemail.com

# INSTALL GLOBAL NPM PACKAGES
RUN sudo yarn global add typescript \
                         tslint

RUN git clone https://github.com/Shougo/vimproc.vim.git $HOME/.vim/bundle/vimproc.vim  && \
    cd $HOME/.vim/bundle/vimproc.vim                                                   && \
    make

# ADD VIM CONFIGURATION
ADD home $HOME
RUN cd $HOME/.vim                                                                   && \
    echo source $HOME/.vim/tsPlugins.vim | sudo tee -a languageSpecificPlugins.vim  && \
    echo source $HOME/.vim/tsConfig.vim  | sudo tee -a languageSpecificConfig.vim

# INSTALL VIM PLUGINS
RUN vim -u $HOME/.vim/plugins.vim +PluginInstall +qall

# INSTALL PHANTOM JS DEPENDENCIES
RUN sudo apt-get install -y libfontconfig
