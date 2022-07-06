Imports System.Collections.Generic
Imports Framesharp.Configuration
Imports StructureMap.Configuration.DSL

''' <summary>
''' This class defines a common place for defining global application settings
'''  and for facading access to the configuration manager and .config settings</summary>
''' <remarks></remarks>
Public Class RootApplicationSettings
    Inherits ApplicationSettings

    Public Sub New()

        InitializeClass()

    End Sub

    ''' <summary>
    ''' Class initialization method
    ''' </summary>
    Public Sub InitializeClass()

        '' Defines which IoC registry classes will be loaded.
        DependencyRegistries = New List(Of Registry)() From
                {
                    {New Framesharp.DependencyInjection.Registry.NHibernateRegistry(DatabaseSettings.BuildSessionFactory())},
                    {New Framesharp.DependencyInjection.Registry.CoreRegistry()},
                    {New Framesharp.DependencyInjection.Registry.DomainServiceRegistry()},
                    {New Framesharp.DependencyInjection.Registry.RepositoryRegistry()},
                    {New DependencyInjectionRegistry()}
                }
    End Sub

End Class