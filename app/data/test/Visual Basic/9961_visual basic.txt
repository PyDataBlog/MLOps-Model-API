Imports System.Runtime.Serialization

<DataContract>
Public Class StatusObject

    ''' <summary>
    ''' ささやいた日時を取得します
    ''' </summary>
    ''' <remarks></remarks>
    Public ReadOnly Property CreatedAt As DateTime
        Get
            Return DatetimeParser.Parse(cat)
        End Get
    End Property

    <DataMember(Name:="created_at")>
    Private Property cat As String

    ''' <summary>
    ''' お気に入り済みかを取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="favorited")>
    Public Property IsFavorited As Boolean?

    ''' <summary>
    ''' お気に入り数を取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="favorited_count")>
    Public Property FavoritedCount As Decimal?

    ''' <summary>
    ''' ステータスidを取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="id")>
    Public Property Id As Decimal?

    ''' <summary>
    ''' 返信先のスクリーンネームを取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="in_reply_to_screen_name")>
    Public Property InReplyToScreenName As String

    ''' <summary>
    ''' 返信先のステータスidを取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="in_reply_to_status_id")>
    Public Property InReplyToStatusId As Decimal?

    ''' <summary>
    ''' 返信先のユーザーidを取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="in_reply_to_user_id")>
    Public Property InReplyToUserId As Decimal?

    ''' <summary>
    ''' スプレッド数を取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="spread_count")>
    Public Property SpreadCount As Decimal?

    ''' <summary>
    ''' スプレッド済みかを取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="spread")>
    Public Property IsSpread As Boolean?

    ''' <summary>
    ''' スプレッド元のStatusObjectを取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="spread_status")>
    Public Property SpreadStatus As StatusObject

    ''' <summary>
    ''' 引用元のStatusObjectを取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="reply_status")>
    Public Property ReplyStatus As StatusObject

    ''' <summary>
    ''' ささやき元のクライアントのSourceObjectを取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="source")>
    Public Property Source As SourceObject

    ''' <summary>
    ''' ささやきの本文を取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="text")>
    Public Property Text As String

    ''' <summary>
    ''' ささやき元ユーザーのUserObjectを取得します
    ''' </summary>
    ''' <remarks></remarks>
    <DataMember(Name:="user")>
    Public Property User As UserObject

    ''' <summary>
    ''' Entitiesを取得します
    ''' </summary>
    ''' <value></value>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <DataMember(Name:="entities")>
    Public Property Entities As EntitiesObject
End Class

Public Class StatusObjectList
    Inherits List(Of StatusObject)
End Class
