CXX ?= g++
CXXFLAGS ?= -Wall -g
# source and objcet directorys
SRC_DIR ?= src
OBJ_DIR ?= objs
# name of executable (program)
execable ?= labyrinthus
# where to copy the exec
install_dir ?= ~/execs
# autodetect *.cpp files
CPPS := $(wildcard $(SRC_DIR)/*.cpp)
# all object files
OBJS := $(addprefix $(OBJ_DIR)/,$(notdir $(CPPS:.cpp=.o)))

sdl: setSDL _all
qt: setQT _all

setSDL:
	$(eval GUILIB := SDLGUI)
	@echo "---------------------------"
	@echo -e "\e[36m SDL GUI\e[0m\n---------------------------"
setQT:
	$(eval GUILIB := QTGUI)
	@echo "---------------------------"
	@echo -e "\e[35m QT GUI\e[0m\n---------------------------"

_all: makeODir $(execable)

makeODir:
	@mkdir -p $(OBJ_DIR)
$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cpp
	@echo -e "\e[1;33m$(CXX) $(CXXFLAGS) -c $< -o $@\e[0m"
	@$(CXX) $(CXXFLAGS) -c $< -o $@
$(execable): $(OBJS)
	@echo -e "\e[1;32m$(CXX) $(CXXFLAGS) <obj_files> -o $@ $(LIBS)\e[0m"
	@$(CXX) $(CXXFLAGS) -D $(GUILIB) $(OBJS) -o $@
install: $(execable)
	@echo -en "Installing...\r"
	@cp $(execable) $(install_dir)
	@echo -e '\e[32m$(execable) -> $(install_dir)\e[0m'
clean:
	@rm -rf $(OBJ_DIR)
	@rm -rf $(execable)
	@echo -e "\e[32mCleaned.\e[0m"
help: info
info:
	@echo -e "\nMakefile, to make \e[1;32m$(execable)\e[0m\n"
	@echo -e "------\e[31m Use the following targets \e[0m-----------------"
	@echo -e "\e[35m<None>\e[0m | \e[36msdl\e[0m\n\tto make \e[1;32m$(execable)\e[0m in SDL mode."
	@echo -e "\e[36mqt\e[0m\n\tto make \e[1;32m$(execable)\e[0m in QT mode."
	@echo -e "\e[36minstall\e[0m\n\tto make and install \e[1;32m$(execable)\e[\0m."
	@echo -e "\e[36mclean\e[0m\n\tto cleanup."
	@echo -e "\e[36mhelp\e[0m | \e[36minfo\e[0m\n\tto type this message."
	@echo -e "------\e[31m Setable variables \e[0m-------------------------"
	@echo -e "\e[33mCXX\e[0m\n\tfor set compiler."
	@echo -e "\tby default: '\e[35m$(CXX)\e[0m'"
	@echo -e "\e[33mCXXFLAGS\e[0m\n\tfor set compilation flags."
	@echo -e "\tby default: '\e[35m$(CXXFLAGS)\e[0m'"
	@echo -e "\e[33mexecable\e[0m\n\tfor rename executable (program)."
	@echo -e "\tby default: '\e[32m$(execable)\e[0m'"
	@echo -e "\e[33minstall_dir\e[0m\n\tfor set install directory."
	@echo -e "\tby default: '\e[34m$(install_dir)\e[0m'"
	@echo -e "--------------------------------------------------\n"
	@echo -e "For more information please look into Makefile.\n"
