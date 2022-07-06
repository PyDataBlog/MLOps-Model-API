#!/usr/bin/env python
# -*- coding: utf-8 -*-
import os
import sys

if __name__ == "__main__":
    settings_name = "settings.local" if os.name == 'nt' else "settings.remote"
    os.environ.setdefault("DJANGO_SETTINGS_MODULE", settings_name)

    from django.core.management import execute_from_command_line

    execute_from_command_line(sys.argv)
