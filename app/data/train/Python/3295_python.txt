from django.contrib.auth.models import User
from django.views.generic.edit import CreateView, FormView
from django.shortcuts import render
from django.views.decorators.csrf import csrf_exempt
from django.utils.decorators import method_decorator
from django.core.context_processors import csrf
from django.http import HttpResponse, JsonResponse
from django.contrib.auth.decorators import login_required



def test_page(request):
    return render(request, "test.html")