using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
//for network
using System.Net;
using System.Net.Sockets;

namespace nex1_server
{
    public class EchoServer
    {
        private static byte[] buf = new byte[1024];
        private List<Socket> socketList = new List<Socket>();
        static void Main(string[] args)
        {
            Console.Write("포트번호입력(1024 - 65535): ");
            int port = int.Parse(Console.ReadLine());
            IPEndPoint myAddr = new IPEndPoint(IPAddress.Any, port);
            Socket server = new Socket(AddressFamily.InterNetwork, 
                SocketType.Stream, 
                ProtocolType.Tcp);
            server.Bind(myAddr);
            
            server.Listen(10);
            SocketAsyncEventArgs args = new SocketAsyncEventArgs();
            args.Completed += new EventHandler(Accept_Completed);

            server.AcceptAsync(args);
            Console.WriteLine("서버 시작");
            
            while (true)
            {
                Socket client = server.Accept();
                Console.WriteLine("클라이언트 {0} 접속", 
                    client.RemoteEndPoint.ToString());
                NetworkStream ns = new NetworkStream(client);

                int len;
                string msg;
                while ((len = ns.Read(buf, 0, buf.Length)) != 0)
                {
                    msg = Encoding.UTF8.GetString(buf, 0, len);
                    Console.WriteLine("수신: {0}" , msg);
                    ns.Write(buf, 0, len);
                }
                Console.WriteLine("클라이언트 {0} 접속종료",
                    client.RemoteEndPoint.ToString());
                ns.Close();                
                client.Close();               
            }
        }

        private void Accept_Completed(object sender, SocketAsyncEventArgs e)
        {
            Socket client = e.AcceptSocket;
            
            SocketAsyncEventArgs receiveArgs = new SocketAsyncEventArgs();
            receiveArgs.UserToken = _telegram;
            _receiveArgs.SetBuffer(_telegram.GetBuffer(), 0, 4);
            _receiveArgs.Completed += new EventHandler(Recieve_Completed);
            _client.ReceiveAsync(_receiveArgs);
            m_Client.Add(_client);
            SendDisplay("Client Connection!", ChatType.System);

            Socket _server = (Socket)sender;
            e.AcceptSocket = null;
            _server.AcceptAsync(e);
        }
    }
}
