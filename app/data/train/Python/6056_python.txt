#!/usr/bin/env python

"""
The pyligadb module is a small python wrapper for the OpenLigaDB webservice.

The pyligadb module has been released as open source under the MIT License.
Copyright (c) 2014 Patrick Dehn

Due to suds, the wrapper is very thin, but the docstrings may be helpful.
Most of the methods of pyligadb return a list containing the requested data as
objects. So the attributes of the list items are accessible via the dot notation
(see example below). For a more detailed description of the return values see
the original documentation: http://www.openligadb.de/Webservices/Sportsdata.asmx

Example use (prints all matches at round 14 in season 2010 from the Bundesliga):
>>> from pyligadb.pyligadb import API
>>> matches = API().getMatchdataByGroupLeagueSaison(14, 'bl1', 2010)
>>> for match in matches:
>>>    print u"{} vs. {}".format(match.nameTeam1, match.nameTeam2)
1. FSV Mainz 05 vs. 1. FC Nuernberg
1899 Hoffenheim vs. Bayer Leverkusen
...
...
"""
__version__ = "0.1.1"

try:
    from suds.client import Client
except ImportError:
    raise Exception("pyligadb requires the suds library to work. "
        "https://fedorahosted.org/suds/")

class API:
    def __init__(self):
        self.client = Client('http://www.openligadb.de/Webservices/'
            'Sportsdata.asmx?WSDL').service
        
    def getAvailGroups(self, leagueShortcut, leagueSaison):
        """
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @param leagueSaison: A specific season (i.e. the date 2011 as integer)
        @return: A list of available groups (half-final, final, etc.) for the
            specified league and season.
        """
        return self.client.GetAvailGroups(leagueShortcut, leagueSaison)[0]

    def getAvailLeagues(self):
        """
        @return: A list of all in OpenLigaDB available leagues.
        """
        return self.client.GetAvailLeagues()[0]

    def getAvailLeaguesBySports(self, sportID):
        """
        @param sportID: The id related to a specific sport.
            Use getAvailSports() to get all IDs.
        @return: A list of all in OpenLigaDB available leagues of the specified
            sport.
        """
        return self.client.GetAvailLeaguesBySports(sportID)[0]

    def getAvailSports(self):
        """
        @return: An object containing all in OpenLigaDB available sports.
        """
        return self.client.GetAvailSports()[0]
    
    def getCurrentGroup(self, leagueShortcut):
        """
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @return: An object containing information about the current group for
            the specified league (i.e. the round ("Spieltag") of the German
            Bundesliga).
        """
        return self.client.GetCurrentGroup(leagueShortcut)
        
    def getCurrentGroupOrderID(self, leagueShortcut):
        """
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @return: The current group-ID for the specified league
            (see getCurrentGroup()) as int value.
        """
        return self.client.GetCurrentGroupOrderID(leagueShortcut)
        
    def getGoalGettersByLeagueSaison(self, leagueShortcut, leagueSaison):
        """
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @param leagueSaison: A specific season (i.e. the date 2011 as integer).
        @return: A list of scorers from the specified league and season, sorted
            by goals scored.
        """
        return self.client.GetGoalGettersByLeagueSaison(leagueShortcut,
            leagueSaison)[0]
    
    def getGoalsByLeagueSaison(self, leagueShortcut, leagueSaison):
        """
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @param leagueSaison: A specific season (i.e. the date 2011 as integer).
        @return: A list of all goals from the specified league and season.
        """
        return self.client.GetGoalsByLeagueSaison(leagueShortcut,
            leagueSaison)[0]

    def getGoalsByMatch(self, matchID):
        """
        @param matchID: The ID of a specific Match. Use i.e. getLastMatch() to
            obtain an ID.
        @return: A list of all goals from the specified match or None.
        """
        result = self.client.GetGoalsByMatch(matchID)
        if result == "":
            return None
        else:
            return result[0]
        
    def getLastChangeDateByGroupLeagueSaison(self, groupOrderID, leagueShortcut,
        leagueSaison):
        """
        @param groupOrderID: The id of a specific group.
            Use i.e. getCurrentGroupOrderID() to obtain an ID.
        @param leagueShortcut: Shortcut for a specific leagueself.
            Use getAvailLeagues() to get all shortcuts.
        @param leagueSaison: A specific season (i.e. the date 2011 as integer).
        @return: The date of the last change as datetime object.
        """
        return self.client.GetLastChangeDateByGroupLeagueSaison(groupOrderID,
            leagueShortcut, leagueSaison)

    def getLastChangeDateByLeagueSaison(self, leagueShortcut, leagueSaison):
        """
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @param leagueSaison: A specific season (i.e. the date 2011 as integer).
        @return: The date of the last change as datetime object.
        """
        return self.client.GetLastChangeDateByLeagueSaison(leagueShortcut,
            leagueSaison)
        
    def getLastMatch(self, leagueShortcut):
        """
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @return: An object containing information about the last match from the
            specified league.
        """
        return self.client.GetLastMatch(leagueShortcut)
    
    def getLastMatchByLeagueTeam(self, leagueID, teamID):
        """
        @param leagueID: Shortcut for a specific league.
            Use getAvailLeagues() to get all IDs.
        @param teamID: The ID of a team, which cab be obtained by using
            getTeamsByLeagueSaison()
        @return: An object containing information about the last played match 
        """
        return self.client.GetLastMatchByLeagueTeam(leagueID, teamID)

    def getMatchByMatchID(self, matchID):
        """
        @param matchID: The ID of a specific Match. Use i.e. getNextMatch()
            to obtain an ID.
        @return: An object containing information about the specified match.
        """
        return self.client.GetMatchByMatchID(matchID)
    
    def getMatchdataByGroupLeagueSaison(self, groupOrderID, leagueShortcut,
        leagueSaison):
        """
        @param groupOrderID: The ID of a specific group.
            Use i.e. getCurrentGroupOrderID() to obtain an ID.
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @param leagueSaison: A specific season (i.e. the date 2011 as integer).
        @return: A list of matches. Each list item is an object containing
            detailed information about the specified group/round.
        """
        return self.client.GetMatchdataByGroupLeagueSaison(groupOrderID,
            leagueShortcut, leagueSaison)[0]

    def getMatchdataByGroupLeagueSaisonJSON(self, groupOrderID, leagueShortcut,
        leagueSaison):
        """
        @param groupOrderID: The ID of a specific group.
            Use i.e. getCurrentGroupOrderID() to obtain an ID.
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @param leagueSaison: A specific season (i.e. the date 2011 as integer).
        @return: A JSON-Object containing detailed information about the
            specified group/round.
        """
        return self.client.GetMatchdataByGroupLeagueSaison(groupOrderID,
            leagueShortcut, leagueSaison)
    
    def getMatchdataByLeagueDateTime(self, fromDateTime, toDateTime,
        leagueShortcut):
        """
        @param fromDateTime: limit the result to matches later than fromDateTime
        @type fromDateTime: datetime.datetime
        @param toDateTime: limit the result to matches earlier than toDateTime
        @type toDateTime: datetime.datetime
        @return: A list of matches in the specified period.
        """
        return self.client.GetMatchdataByLeagueDateTime(fromDateTime,
            toDateTime, leagueShortcut)[0]
        
    def getMatchdataByLeagueSaison(self, leagueShortcut, leagueSaison):
        """
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @param leagueSaison: A specific season (i.e. the date 2011 as integer).
        @return: A list of all matches in the specified league and season.
        @note: May take some time...
        """
        return self.client.GetMatchdataByLeagueSaison(leagueShortcut,
            leagueSaison)[0]

    def getMatchdataByTeams(self, teamID1, teamID2):
        """
        @param teamID1: ID of the first team. Use i.e. getTeamsByLeagueSaison()
            to obtain team IDs.
        @param teamID1: ID of the second team.
        @return: A list of matches, at which the specified teams play against
            each other.
        """
        return self.client.GetMatchdataByTeams(teamID1, teamID2)[0]

    def getNextMatch(self, leagueShortcut):
        """
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @return: An object containing information about the next match from the
            specified league.
        """
        return self.client.GetNextMatch(leagueShortcut)
    
    def getNextMatchByLeagueTeam(self, leagueID, teamID):
        """
        @param leagueID: Shortcut for a specific league.
            Use getAvailLeagues() to get all IDs.
        @param teamID: The ID of a team, which cab be obtained by using
            getTeamsByLeagueSaison()
        @return: An object containing information about the next match
        """
        return self.client.GetNextMatchByLeagueTeam(leagueID, teamID)

    def getTeamsByLeagueSaison(self, leagueShortcut, leagueSaison):
        """
        @param leagueShortcut: Shortcut for a specific league.
            Use getAvailLeagues() to get all shortcuts.
        @param leagueSaison: A specific season (i.e. the date 2011 as integer).
        @return: A list of all teams playing in the specified league and season.
        """
        return self.client.GetTeamsByLeagueSaison(leagueShortcut,
            leagueSaison)[0]