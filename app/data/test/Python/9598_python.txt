# -*- coding: utf-8 -*-
from . import app, db
from flask import request, g, session, redirect
from Lotus.model.user import User
from hashlib import md5
from Lotus.lib.msg_code import Msg
import json


@app.route('/user/login', methods=['POST'])
def user_login():
    email = request.form.get('email', None)
    psw = request.form.get('psw', None)
    if email is not None and psw is not None:
        users = User.query.filter_by(email=email, psw=psw)
        if users:
            g.user = users[0]
            session['userid'] = users[0].userid
        else:
            return '{"code":%d,"msg":$s}'.format(Msg['faild'], 'user not exist')

    else:
        return '{"code":%d,"msg":$s}'.format(Msg['faild'], 'params not enougth')


@app.route('/user/register', methods=['POST'])
def user_register():
    # todo (参数不够)有插入异常怎么办？
    # todo 忘记密码..
    try:
        u = User()
        u.username = request.form.get('username', None)
        u.description = request.form.get('description', None)
        u.type = request.form.get('type', User.CONST_TYPE_USER)
        u.email = request.form.get('email', None)
        m = md5()
        m.update(request.form.get('psw', User.CONST_DEFAULT_PASSWORD))  # 默认密码
        u.psw = m.hexdigest()
        db.session.add(u)
        db.session.commit()
    except Exception as e:
        return '{"code":%d,"msg":$s}'.format(Msg['faild'], 'register faild')
    return '{"code":%d,"msg":$s}'.format(Msg['success'], 'register success')


@app.route('/user/<int:userid>/avatar', methods=['GET', 'POST'])
def user_avatar(userid):
    #upload
    #TODO support upload avater
    if request.method == 'POST':
        pass
    else:
        pass


@app.route('/user/<int:userid>/profile', methods=['GET'])
def user_profile(userid):
    if session.get('userid'):
        result = {
            'userid': g.user.userid,
            'username': g.user.username,
            'avatar': g.user.avatar,
            'description': g.user.description,
            'type': g.user.type,
            'email': g.user.email
        }
        return json.dumps(result)
    else:
        redirect('/user/login')


@app.route('/user/<int:userid>/issue/sends/page/<int:page>', methods=['GET'])
def user_issues_send(userid, page):
    pass


@app.route('/user/<int:userid>/issue/favours/page/<int:page>', methods=['GET'])
def user_issues_favour(userid, page):
    pass


@app.route('/user/<int:userid>/issue/favours/page/<int:page>', methods=['GET'])
def user_messages(userid, page):
    pass



