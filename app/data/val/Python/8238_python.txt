from django.views.generic import ListView
from models import Project
# Create your views here.

class ListProjectView(ListView):
	model = Project
	template_name = 'cmsplugin_vfoss_project/project_list.html'
