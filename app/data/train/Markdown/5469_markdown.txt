---
title: Net4Email
menu: Home
onpage_menu: true
body_class: index
header_class: alt
recaptchacontact: true
content:
    items: @self.modular
    order:
        by: default
        dir: asc
        custom:
            - _banner
            - _icons
            - _header
            - _features
            - _portfolio
            - _contact
process:
  twig: true
cache_enable: false

form:
    name: contacto
    action : /
    fields:
    -
        name: nombre
        label: Nombre completo:
        placeholder: 'Ingrese ambos nombres'
        autofocus: 'on'
        autocomplete: 'on'
        type: text
        
    -
        name: empresa
        label: Empresa:
        type: text
        
    -
        name: correo
        label: Correo Electrónico:
        type: email
        validate:
            required : true
        
    - 
      name: telefono
      label: Telefono Oficina:
      placeholder: 212-222-2222
      type: text
      
    - 
      name: celular
      label: Telefono Celular:
      placeholder: (41X) 212-222-2222
      type: text
      
    - 
      name: requerimiento
      label: Requerimiento de su solicitud:
      type: text
      
    -
      type: select
      name: requerimiento
      label: Requerimiento de su solicitud
      options:
        default: Solicitar Informacion
        demo: Solicitar un demo del Producto
      
    - 
      name: g-recaptcha-response
      label: Captcha
      type: captcha
      recaptcha_site_key: 6LctJAwUAAAAACbdnqNINEvNq7MGcrXD2wU7o7UI
      recaptcha_not_validated: 'Captcha not valid!'
      validate:
        required: true
      
    -  
      name: comentario
      label: Comentarios:
      placeholder: comentario,preguntas o dudas con respecto a net4email
      autofocus: true
      type: textarea
 
        
    buttons:
    -
        type : submit
        value : Enviar

    process:
        - message: La información ha sido enviada...un representante de netquatro se comunicará con usted
---

