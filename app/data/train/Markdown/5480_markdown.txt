---
layout: post
title: "쉽게 배우는 Web App 5 : Web App에 Cognitive API를 연결해보자!"
featimg: azure-logo.jpg
categories: [Web Application]
tag: [webapp]
---

1. 새로만들기를 클릭합니다.
![그림](/img/webapp/cognitive/login_success.png)
2. 다양한 Cognitive Service가 있습니다. 여기서 우리는 Translator Text API를 써보겠습니다.
![그림](/img/webapp/cognitive/kind.PNG)
3. 다음과 같이 빈 칸을 모두 입력하고 만들기를 클릭합니다.
![그림](/img/webapp/cognitive/create.png)
4. 만들어 지면 key값을 기억합니다.
![그림](/img/webapp/cognitive/key.png)
5. 다음과 같이 코드를 만듭니다.
	1. `auth.py`
	```
    from datetime import timedelta
	from datetime import datetime
	import requests
       class AzureAuthClient(object):
           def __init__(self, client_secret):
               self.client_secret = client_secret
               self.token = None
               self.reuse_token_until = None
           def get_access_token(self):
               if (self.token is None) or (datetime.utcnow() > self.reuse_token_until):
                   token_service_url = 'https://api.cognitive.microsoft.com/sts/v1.0/issueToken'
                   request_headers = {'Ocp-Apim-Subscription-Key': self.client_secret}
                   response = requests.post(token_service_url, headers=request_headers)
                   response.raise_for_status()
                   self.token = response.content
                   self.reuse_token_until = datetime.utcnow() + timedelta(minutes=5)
               return self.token
    ```
    2. `translate.py`
    ```
    	from xml.etree import ElementTree
        from auth import AzureAuthClient
        import requests
        def GetTextAndTranslate(finalToken):
            toLangCode = 'ko'
            textToTranslate = " "
            textToTranslate = input("Type the text that you want to translate:  ")
            # Call to Microsoft Translator Service
            headers = {"Authorization ": finalToken}
            translateUrl = "http://api.microsofttranslator.com/v2/Http.svc/Translate?text={}&to={}".format(textToTranslate, toLangCode)
            translationData = requests.get(translateUrl, headers=headers)
            # parse xml return values
            translation = ElementTree.fromstring(translationData.text.encode('utf-8'))
            # display translation
            print(translation.text)
        if __name__ == "__main__":
            client_secret = 'YOUR_KEY'
            auth_client = AzureAuthClient(client_secret)
            bearer_token = b'Bearer ' + auth_client.get_access_token()
            GetTextAndTranslate(bearer_token)
    ```
6. 다음과 같은 결과를 볼 수 있다.
![그림](/img/webapp/cognitive/result.png)

# Source Code
* [https://github.com/Azure-For-Beginner/azure-webapp-sample-code/tree/cognitive](https://github.com/Azure-For-Beginner/azure-webapp-sample-code/tree/cognitive)