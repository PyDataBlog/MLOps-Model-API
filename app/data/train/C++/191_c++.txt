#include <algorithm>
#include <iostream>
#include "RustyFist/DrawMe.h"
#include "RustyFist/TouchSink.h"
#include "OpenGLLayer.h"

using namespace cocos2d;
using namespace std;

OpenGLLayer::OpenGLLayer()
{
}

OpenGLLayer::~OpenGLLayer()
{
}

bool OpenGLLayer::init()
{
	return Layer::init();
}

cocos2d::Scene* OpenGLLayer::scene(DrawMe* drawMe, TouchSink* ts)
{
	auto scene = Scene::create();

	OpenGLLayer *layer = OpenGLLayer::create();
	layer->setDrawMe(drawMe);
	layer->setTouchSink(ts);

	scene->addChild(layer);

	return scene;
}

void OpenGLLayer::draw(cocos2d::Renderer* renderer, const cocos2d::Mat4& transform, uint32_t flags)
{
	if (_drawMe)
		_drawMe->draw();
}

void OpenGLLayer::setTouchSink(TouchSink* ts)
{
	_ts = ts;
	auto mouseEvents = EventListenerMouse::create();

	mouseEvents->onMouseDown = [this](Event* e)
	{
		if(auto me = dynamic_cast<EventMouse*>(e))
		{
			if(_ts)
				_ts->mouse({me->getCursorX(), me->getCursorY()});
		}
	};
	mouseEvents->onMouseUp = [this](Event* e)
	{
		if(auto me = dynamic_cast<EventMouse*>(e))
		{
			if(_ts)
				_ts->mouse({me->getCursorX(), me->getCursorY()});
		}
	};
	mouseEvents->onMouseMove = [this](Event* e)
	{
		if(auto me = dynamic_cast<EventMouse*>(e))
		{
			if(_ts)
				_ts->mouse({me->getCursorX(), me->getCursorY()});
		}
	};

	_eventDispatcher->addEventListenerWithSceneGraphPriority(mouseEvents, this);
}

void OpenGLLayer::onTouchesBegan(const std::vector<cocos2d::Touch*>& touches, cocos2d::Event* unused_event)
{
	sendTouch(touches);
}

void OpenGLLayer::onTouchesMoved(const std::vector<cocos2d::Touch*>& touches, cocos2d::Event* unused_event)
{
	sendTouch(touches);
}

void OpenGLLayer::onTouchesEnded(const std::vector<cocos2d::Touch*>& touches, cocos2d::Event* unused_event)
{
	sendTouch(touches);
}

void OpenGLLayer::onTouchesCancelled(const std::vector<cocos2d::Touch*>& touches, cocos2d::Event* unused_event)
{
	sendTouch(touches);
}

void OpenGLLayer::sendTouch(const std::vector<cocos2d::Touch*>& touches)
{
	std::vector<::Touch> tochs;
	std::transform(touches.begin(), touches.end(), back_inserter(tochs), [](cocos2d::Touch* t)
		{
			return ::Touch{t->getID(), t->getLocation().x, t->getLocation().y};
		});
	if(_ts)
		_ts->touch(tochs);
}
