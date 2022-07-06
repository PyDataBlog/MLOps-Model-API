from rest_framework import serializers
from . import models


class Invoice(serializers.ModelSerializer):
    class Meta:
        model = models.Invoice
        fields = (
            'id', 'name', 'additional_infos', 'owner',
            'creation_date', 'update_date',
        )
