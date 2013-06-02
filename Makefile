# Recursive wildcard function
rwildcard=$(wildcard $1$2) $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2))

# Location of project files
JAVA_DIR    := priv/java
LIB_DIR     := $(JAVA_DIR)/lib
OUTPUT_DIR  := $(JAVA_DIR)/classes
SOURCE_DIR  := java_src
CONFIG_DIR  := config
JAVA_FILES  := $(call rwildcard,$(SOURCE_DIR)/,*.java)

# Distribution JAR file
DIST_JAR    := $(LIB_DIR)/edrul.jar

# Unix tools
AWK         := awk
FIND        := find
MKDIR       := mkdir -p
RM          := rm -rf
SHELL       := /bin/bash

# Java tools
JAVA        := java
JAVAC       := javac
JFLAGS      := -sourcepath $(SOURCE_DIR)        \
               -d $(OUTPUT_DIR)                 \
	       -source 1.5 -Xlint:unchecked
JVMFLAGS    := -ea                              \
               -esa                             \
               -Xfuture
DROOLS_FLAGS := -Ddrools.schema.validating=false
JVM         := $(JAVA) $(JVMFLAGS) $(DROOLS_FLAGS)
JAR         := jar
JARFLAGS    := cf

# Library JARS
3RD_PARTY_JARS   := $(call rwildcard,$(LIB_DIR)/,*.jar)
# Find JInterface OTP JAR
OTP_JAR          := $(shell erl -noshell -eval 'L = case code:priv_dir(jinterface) of {error,_} -> []; P when is_list(P) -> [P ++ "/OtpErlang.jar"] end, io:format("~s~n", [L])' -s erlang halt)

# $(call build-classpath, variable-list)
define build-classpath
$(strip                                         \
  $(patsubst :%,%,                              \
    $(subst : ,:,                               \
      $(strip                                   \
        $(foreach j,$1,$j:)))))
endef

# Set the Java CLASSPATH
class_path := $(3RD_PARTY_JARS) \
             $(CONFIG_DIR)

export CLASSPATH := $(call build-classpath, $(class_path))

# all - Perform all tasks for a complete build
.PHONY: all
all:
	@$(MAKE) jdist
	@$(MAKE) ecompile

.PHONY: clean
clean: jclean eclean

.PHONY: cleanall
cleanall: jcleanall ecleanall

# compile - Compile the source
.PHONY: jcompile
jcompile:
	@echo ==\> Building JAVA
	@$(MKDIR) $(OUTPUT_DIR)
	@$(JAVAC) $(JFLAGS) $(JAVA_FILES)

.PHONY: jclean
jclean:
	@$(RM) $(OUTPUT_DIR)

.PHONY: jcleanall
jcleanall: jclean
	@$(RM) $(DIST_JAR)
	@$(RM) $(JAVA_DIR)/bin/jnode.pid
	@$(RM) $(JAVA_DIR)/log/*.log

.PHONY: jdist
jdist: jcleanall jcompile
	@$(JAR) $(JARFLAGS) $(DIST_JAR) -C $(OUTPUT_DIR) pl
	@echo "JAR file created: $(DIST_JAR)"
	@$(MAKE) jclean

# ENV variable checking helper target
guard-(%):
	@if [ "${${*}}" == "" ]; then \
	    echo "Environment variable $* not set"; \
	    exit 1; \
	fi

.PHONY: ecompile
ecompile:
	@echo ==\> Building ERLANG
	@rebar compile

.PHONY: ecompile_fast
ecompile_fast:
	@echo ==\> Building ERLANG
	@rebar compile skip_deps=true

.PHONY: eclean
eclean:
	@rebar clean

.PHONY: ecleanall
ecleanall: eclean
	@rebar delete-deps
	@$(RM) log/*.log

.PHONY: test
test: ecompile
	@rebar eunit skip_deps=true

init: ecleanall
	@cp $(OTP_JAR) $(LIB_DIR)
	@rebar get-deps

run:
	@rebar clean compile
	@erl -pa deps/*/ebin -pa ../edrul/ebin -sname rating -s edrul -setcookie secret

