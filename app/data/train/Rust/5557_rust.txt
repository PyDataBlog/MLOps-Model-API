// This file is generated. Do not edit
// @generated

// https://github.com/Manishearth/rust-clippy/issues/702
#![allow(unknown_lints)]
#![allow(clippy)]

#![cfg_attr(rustfmt, rustfmt_skip)]

#![allow(box_pointers)]
#![allow(dead_code)]
#![allow(missing_docs)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(trivial_casts)]
#![allow(unsafe_code)]
#![allow(unused_imports)]
#![allow(unused_results)]


// interface

pub trait Communicator {
    fn initiate_conversation(&self, o: ::grpc::RequestOptions, p: super::message::InitiateRequest) -> ::grpc::SingleResponse<super::message::InitiateReply>;

    fn terminate_conversation(&self, o: ::grpc::RequestOptions, p: super::message::TerminateRequest) -> ::grpc::SingleResponse<super::message::TerminateReply>;

    fn send_message(&self, o: ::grpc::RequestOptions, p: ::grpc::StreamingRequest<super::message::MessageRequest>) -> ::grpc::StreamingResponse<super::message::MessageReply>;
}

// client

pub struct CommunicatorClient {
    grpc_client: ::grpc::Client,
    method_InitiateConversation: ::std::sync::Arc<::grpc::method::MethodDescriptor<super::message::InitiateRequest, super::message::InitiateReply>>,
    method_TerminateConversation: ::std::sync::Arc<::grpc::method::MethodDescriptor<super::message::TerminateRequest, super::message::TerminateReply>>,
    method_SendMessage: ::std::sync::Arc<::grpc::method::MethodDescriptor<super::message::MessageRequest, super::message::MessageReply>>,
}

impl CommunicatorClient {
    pub fn with_client(grpc_client: ::grpc::Client) -> Self {
        CommunicatorClient {
            grpc_client: grpc_client,
            method_InitiateConversation: ::std::sync::Arc::new(::grpc::method::MethodDescriptor {
                name: "/Communicator/InitiateConversation".to_string(),
                streaming: ::grpc::method::GrpcStreaming::Unary,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
            method_TerminateConversation: ::std::sync::Arc::new(::grpc::method::MethodDescriptor {
                name: "/Communicator/TerminateConversation".to_string(),
                streaming: ::grpc::method::GrpcStreaming::Unary,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
            method_SendMessage: ::std::sync::Arc::new(::grpc::method::MethodDescriptor {
                name: "/Communicator/SendMessage".to_string(),
                streaming: ::grpc::method::GrpcStreaming::Bidi,
                req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
            }),
        }
    }

    pub fn new(host: &str, port: u16, tls: bool, conf: ::grpc::ClientConf) -> ::grpc::Result<Self> {
        ::grpc::Client::new(host, port, tls, conf).map(|c| {
            CommunicatorClient::with_client(c)
        })
    }
}

impl Communicator for CommunicatorClient {
    fn initiate_conversation(&self, o: ::grpc::RequestOptions, p: super::message::InitiateRequest) -> ::grpc::SingleResponse<super::message::InitiateReply> {
        self.grpc_client.call_unary(o, p, self.method_InitiateConversation.clone())
    }

    fn terminate_conversation(&self, o: ::grpc::RequestOptions, p: super::message::TerminateRequest) -> ::grpc::SingleResponse<super::message::TerminateReply> {
        self.grpc_client.call_unary(o, p, self.method_TerminateConversation.clone())
    }

    fn send_message(&self, o: ::grpc::RequestOptions, p: ::grpc::StreamingRequest<super::message::MessageRequest>) -> ::grpc::StreamingResponse<super::message::MessageReply> {
        self.grpc_client.call_bidi(o, p, self.method_SendMessage.clone())
    }
}

// server

pub struct CommunicatorServer {
    pub grpc_server: ::grpc::Server,
}

impl ::std::ops::Deref for CommunicatorServer {
    type Target = ::grpc::Server;

    fn deref(&self) -> &Self::Target {
        &self.grpc_server
    }
}

impl CommunicatorServer {
    pub fn new<A : ::std::net::ToSocketAddrs, H : Communicator + 'static + Sync + Send + 'static>(addr: A, conf: ::grpc::ServerConf, h: H) -> Self {
        let service_definition = CommunicatorServer::new_service_def(h);
        CommunicatorServer {
            grpc_server: ::grpc::Server::new_plain(addr, conf, service_definition),
        }
    }

    pub fn new_pool<A : ::std::net::ToSocketAddrs, H : Communicator + 'static + Sync + Send + 'static>(addr: A, conf: ::grpc::ServerConf, h: H, cpu_pool: ::futures_cpupool::CpuPool) -> Self {
        let service_definition = CommunicatorServer::new_service_def(h);
        CommunicatorServer {
            grpc_server: ::grpc::Server::new_plain_pool(addr, conf, service_definition, cpu_pool),
        }
    }

    pub fn new_service_def<H : Communicator + 'static + Sync + Send + 'static>(handler: H) -> ::grpc::server::ServerServiceDefinition {
        let handler_arc = ::std::sync::Arc::new(handler);
        ::grpc::server::ServerServiceDefinition::new(
            vec![
                ::grpc::server::ServerMethod::new(
                    ::std::sync::Arc::new(::grpc::method::MethodDescriptor {
                        name: "/Communicator/InitiateConversation".to_string(),
                        streaming: ::grpc::method::GrpcStreaming::Unary,
                        req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                        resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                    }),
                    {
                        let handler_copy = handler_arc.clone();
                        ::grpc::server::MethodHandlerUnary::new(move |o, p| handler_copy.initiate_conversation(o, p))
                    },
                ),
                ::grpc::server::ServerMethod::new(
                    ::std::sync::Arc::new(::grpc::method::MethodDescriptor {
                        name: "/Communicator/TerminateConversation".to_string(),
                        streaming: ::grpc::method::GrpcStreaming::Unary,
                        req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                        resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                    }),
                    {
                        let handler_copy = handler_arc.clone();
                        ::grpc::server::MethodHandlerUnary::new(move |o, p| handler_copy.terminate_conversation(o, p))
                    },
                ),
                ::grpc::server::ServerMethod::new(
                    ::std::sync::Arc::new(::grpc::method::MethodDescriptor {
                        name: "/Communicator/SendMessage".to_string(),
                        streaming: ::grpc::method::GrpcStreaming::Bidi,
                        req_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                        resp_marshaller: Box::new(::grpc::protobuf::MarshallerProtobuf),
                    }),
                    {
                        let handler_copy = handler_arc.clone();
                        ::grpc::server::MethodHandlerBidi::new(move |o, p| handler_copy.send_message(o, p))
                    },
                ),
            ],
        )
    }
}
