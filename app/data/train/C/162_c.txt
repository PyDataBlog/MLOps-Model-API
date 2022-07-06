//
// This is the main include file for the gcode library
// It parses and executes G-code functions
//
// gcode.h
//

#ifndef GCODE_H
#define GCODE_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdarg.h>

#ifndef NRF51
// G-code debug print
unsigned int debug(const char *format, ...);
#else
#include "uart.h"
#endif

// Single G-code parameter
typedef struct {
    char type;
    float value;
} gcode_parameter_t;

// Parse G-code command
int gcode_parse(const char *s);

// Extract G-code parameter (similar to strtok)
int gcode_get_parameter(char **s, gcode_parameter_t *gp);

#endif
