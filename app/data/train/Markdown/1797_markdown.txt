# Authentication

## Introduction

Raptor supports user login and token based authentication in order to handle authentication and authorization to access and use the Raptor APIs.

Users will be able to get credentials for the various entities needing to access specific platform capabilities via Raptor API: credentials are in the form of access tokens, called API Keys, that can be generated and obtained though Raptor user interface.

The following picture explains relationships among users, devices and API keys handled by Raptor.

![API Keys](/img/API_Keys.svg)

## Token

A User can have many Token which can be generated both from the frontend and backend and used in the code.

Tokens can be generated, disabled and deleted, affecting immediately the device or code using that key.

## Role

Roles contains one or more permissions, allowing or denying an user with that role to perform an operation

A User can have many roles.

## Permissions

Permissions allow to have a fine-grained control over what an user can do within the platform

For example a `web application` may have a `PULL`-only permission to access the data of a device. The `device` itself instead will have `PUSH` permission to the data stream API in order to store the data from sensors.

Permission are plain labels that follow a structure:

`read_own_device` is composed of three informations:

* First `read` is the permission. Core permission recognized by the platform are

  * `read`
  * `create`
  * `update`
  * `delete`
  * `push` & `pull` Reference respectively to send and read data for a device
  * `execute` Is the permission to execute a command on a device
  * `admin` is a special flag that means all the permission

* Second is an optional flag to define ownership \(`own`\). This will apply the permission only to the subjects an user created and is considered owner.

  An example is `admin_own_device` which means users can manage devices created by them but no access those created by others.

  Instead `admin_device` will allow users to manage _all_ the devices in the platform.

* Third part is the subject type, in this case `device`. Core types in the platform are

  * `device` A device instance modeled in the platform
  * `user` An user
  * `stream` A collection of data
  * `token` A token used to query the API
  * `client` An oAuth2 client to delegate access

This permission model allow for a certain degree of flexibility in describing permissions on premise. When a more specialized access control is required, the permission API allow to set a specific set of permission of an user on a type instance.

Refer to the Permission API on how to change permissions and delegate access to users

## Tokens

Tokens are used in place of username/password to identify the acting user during the interaction with the platform.

_HTTP requests_: During an HTTP request the token may be prefixed with the `Bearer` keyword and inserted in the `Authorization` header.

A request look like this

```
GET / HTTP/1.1
Host: api.raptor.local
Accept: application/json
Authorization: Bearer <token>
```



