// -*- C++ -*-
/*!
 * @file  SFMLJoystickTest.cpp
 * @brief SFMLJoystick RT-component Test 
 * @date $Date$
 *
 * $Id$
 */

#include "SFMLJoystickTest.h"

// Module specification
// <rtc-template block="module_spec">
static const char* sfmljoysticktest_spec[] =
  {
    "implementation_id", "SFMLJoystickTest",
    "type_name",         "SFMLJoystickTest",
    "description",       "SFMLJoystick RT-component Test ",
    "version",           "1.0.0",
    "vendor",            "SSR",
    "category",          "Humaninterface",
    "activity_type",     "PERIODIC",
    "kind",              "DataFlowComponent",
    "max_instance",      "1",
    "language",          "C++",
    "lang_type",         "compile",
    // Configuration variables
    "conf.default.debug", "0",
    // Widget
    "conf.__widget__.debug", "text",
    // Constraints
    ""
  };
// </rtc-template>

/*!
 * @brief constructor
 * @param manager Maneger Object
 */
SFMLJoystickTest::SFMLJoystickTest(RTC::Manager* manager)
    // <rtc-template block="initializer">
  : RTC::DataFlowComponentBase(manager),
    m_axisIn("axis", m_axis),
    m_buttonsIn("buttons", m_buttons)

    // </rtc-template>
{
}

/*!
 * @brief destructor
 */
SFMLJoystickTest::~SFMLJoystickTest()
{
}



RTC::ReturnCode_t SFMLJoystickTest::onInitialize()
{
  // Registration: InPort/OutPort/Service
  // <rtc-template block="registration">
  // Set InPort buffers
  addInPort("axis", m_axisIn);
  addInPort("buttons", m_buttonsIn);
  
  // Set OutPort buffer
  
  // Set service provider to Ports
  
  // Set service consumers to Ports
  
  // Set CORBA Service Ports
  
  // </rtc-template>

  // <rtc-template block="bind_config">
  // Bind variables and configuration variable
  bindParameter("debug", m_debug, "0");
  // </rtc-template>
  
  return RTC::RTC_OK;
}

/*
RTC::ReturnCode_t SFMLJoystickTest::onFinalize()
{
  return RTC::RTC_OK;
}
*/

/*
RTC::ReturnCode_t SFMLJoystickTest::onStartup(RTC::UniqueId ec_id)
{
  return RTC::RTC_OK;
}
*/

/*
RTC::ReturnCode_t SFMLJoystickTest::onShutdown(RTC::UniqueId ec_id)
{
  return RTC::RTC_OK;
}
*/

/*
RTC::ReturnCode_t SFMLJoystickTest::onActivated(RTC::UniqueId ec_id)
{
  return RTC::RTC_OK;
}
*/

/*
RTC::ReturnCode_t SFMLJoystickTest::onDeactivated(RTC::UniqueId ec_id)
{
  return RTC::RTC_OK;
}
*/


RTC::ReturnCode_t SFMLJoystickTest::onExecute(RTC::UniqueId ec_id)
{
  if (m_axisIn.isNew()) {
    m_axisIn.read();
  } 

  if (m_buttonsIn.isNew()) {
    m_buttonsIn.read();
  }


  std::cout << "[RTC::SFMLJoystickTest] AXIS: ";
  for (int i = 0;i < m_axis.data.length();i++) {
    std::cout << m_axis.data[i];
    if (i != m_axis.data.length() -1) {
      std::cout << ", ";
    }
  }
  std::cout << "\n";

  std::cout << "[RTC::SFMLJoystickTest] BTN : ";
  for (int i = 0;i < m_buttons.data.length();i++) {
    std::cout << m_buttons.data[i];
    if (i != m_buttons.data.length() -1) {
      std::cout << ", ";
    }
  }
  std::cout << "\n";
  
  return RTC::RTC_OK;
}


/*
RTC::ReturnCode_t SFMLJoystickTest::onAborting(RTC::UniqueId ec_id)
{
  return RTC::RTC_OK;
}
*/

/*
RTC::ReturnCode_t SFMLJoystickTest::onError(RTC::UniqueId ec_id)
{
  return RTC::RTC_OK;
}
*/

/*
RTC::ReturnCode_t SFMLJoystickTest::onReset(RTC::UniqueId ec_id)
{
  return RTC::RTC_OK;
}
*/

/*
RTC::ReturnCode_t SFMLJoystickTest::onStateUpdate(RTC::UniqueId ec_id)
{
  return RTC::RTC_OK;
}
*/

/*
RTC::ReturnCode_t SFMLJoystickTest::onRateChanged(RTC::UniqueId ec_id)
{
  return RTC::RTC_OK;
}
*/



extern "C"
{
 
  void SFMLJoystickTestInit(RTC::Manager* manager)
  {
    coil::Properties profile(sfmljoysticktest_spec);
    manager->registerFactory(profile,
                             RTC::Create<SFMLJoystickTest>,
                             RTC::Delete<SFMLJoystickTest>);
  }
  
};


