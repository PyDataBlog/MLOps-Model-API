# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from django.db import models, migrations


class Migration(migrations.Migration):

    dependencies = [
    ]

    operations = [
        migrations.CreateModel(
            name='Page',
            fields=[
                ('id', models.AutoField(verbose_name='ID', serialize=False, auto_created=True, primary_key=True)),
                ('title', models.CharField(unique=True, max_length=150)),
                ('slug', models.SlugField(unique=True, max_length=150)),
                ('posted', models.DateTimeField(auto_now_add=True, db_index=True)),
            ],
            options={
            },
            bases=(models.Model,),
        ),
    ]
