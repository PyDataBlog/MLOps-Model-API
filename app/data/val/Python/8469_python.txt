import json
from decimal import Decimal


class CustomEncoder(json.JSONEncoder):
    def default(self, object):
        if isinstance(object, set):
            return list(object)
        if isinstance(object, Decimal):
            if object % 1 > 0:
                return float(object)
            else:
                return int(object)
        return super(CustomEncoder, self).default(object)
