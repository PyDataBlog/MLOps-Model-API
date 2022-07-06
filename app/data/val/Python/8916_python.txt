from django.http import HttpResponse
from django.shortcuts import render

from survey.models import Choice
from survey.forms import ChoiceForm

import csv
import random

# Create your views here.

def index(request):
  examples = ['controlling Exposure', 'changing Temperature', 'modifying Highlights', 'changing Shadows', 'Zooming in/out', 'changing the Constrast']

  if request.method == 'POST':
    f = ChoiceForm(request.POST)
    
    if f.is_valid():
      newChoice = f.save()

      if request.session.get('previous_responses', False):
        prev_response_array = request.session['previous_responses']
        prev_response_array.append({'program':newChoice.program, 'text':newChoice.text})
        request.session['previous_responses'] = prev_response_array
      else:
        request.session['previous_responses'] = [{'program':newChoice.program, 'text':newChoice.text}];

      return render(request, 'index.html', {'previous':1, 'previous_responses':request.session['previous_responses'], 'example':random.choice(examples)})

  if request.session.get('previous_responses', False):
    return render(request, 'index.html', {'previous':1, 'previous_responses':request.session['previous_responses'], 'example':random.choice(examples)})
  else:
    return render(request, 'index.html', {'previous':None, 'previous_responses':None, 'example':random.choice(examples)})

def responses(request):
    response = HttpResponse(content_type='text/csv')
    response['Content-Disposition'] = 'attachment; filename="responses.csv"'

    writer = csv.writer(response)
    writer.writerow(['Date', 'Application', 'Suggested Operation'])
    
    for aChoice in Choice.objects.all():
      writer.writerow([aChoice.date, aChoice.program, aChoice.text])

    return response

