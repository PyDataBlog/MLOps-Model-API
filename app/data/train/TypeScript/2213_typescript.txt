class Message {
    public static avatar = ".chat-message-avatar";
    public static avatarImg(userId: string): string {
        return `.avatar_${userId}`;
    }
    public static local(messageId: string): string {
        return `.local${messageId}`;
    }
    public static info = ".chat-message-info";
    public static displayedName = ".chat-name";
    public static guest = ".chat-name-guest";
    public static creator = ".chat-name-creator";
    public static moderator = ".chat-name-moderator";
    public static content = ".chat-message-content";
    public static right = ".chat-message-right";
    public static terrariaStatus = ".chat-terraria-status";
    public static terrariaOnline = ".chat-terraria-online";
    public static terrariaOffline = ".chat-terraria-offline";
}

export default Message;
