using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Linq;
using SirenOfShame.Lib;
using SirenOfShame.Lib.Watcher;
using log4net;

namespace GoServices
{
    public class GoBuildStatus : BuildStatus
    {
        private readonly IEnumerable<XElement> _pipeline;
        private static readonly ILog _log = MyLogManager.GetLogger(typeof(GoBuildStatus));

        public GoBuildStatus(IEnumerable<XElement> pipeline)
        {
            _pipeline = pipeline;

            Name = GetPipelineName();
            BuildDefinitionId = GetPipelineName();
            BuildStatusEnum = GetBuildStatus();
            BuildId = GetBuildId();
            LocalStartTime = GetLocalStartTime();
            Url = GetUrl();

            if (BuildStatusEnum == BuildStatusEnum.Broken)
            {
                RequestedBy = GetRequestedBy();
            }
        }

        private string GetPipelineName()
        {
            return _pipeline.First().Attribute("name").Value.Split(' ').First();
        }

        private BuildStatusEnum GetBuildStatus()
        {
            if (_pipeline.Select(x => x.Attribute("activity").Value).Any(a => a == "Building"))
            {
                return BuildStatusEnum.InProgress;
            }
            if (_pipeline.Select(x => x.Attribute("activity").Value).Any(a => a == "Sleeping") &&
                _pipeline.Select(x => x.Attribute("lastBuildStatus").Value).All(s => s == "Success"))
            {
                return BuildStatusEnum.Working;
            }
            if (_pipeline.Select(x => x.Attribute("activity").Value).Any(a => a == "Sleeping") &&
                _pipeline.Select(x => x.Attribute("lastBuildStatus").Value).Any(s => s == "Failure"))
            {
                return BuildStatusEnum.Broken;
            }

            return BuildStatusEnum.Unknown;
        }

        private string GetBuildId()
        {
            return _pipeline.First().Attribute("lastBuildLabel").Value;
        }

        private DateTime GetLocalStartTime()
        {
            return Convert.ToDateTime(_pipeline.First().Attribute("lastBuildTime").Value);
        }

        private string GetUrl()
        {
            return _pipeline.First().Attribute("webUrl").Value;
        }

        private string GetRequestedBy()
        {
            var failedStage = _pipeline.FirstOrDefault(x => x.Element("messages") != null &&
                                                   x.Element("messages").Element("message") != null);
            try
            {
                return failedStage != null
                           ? failedStage.Element("messages").Element("message").Attribute("text").Value
                           : string.Empty;
            }
            catch (Exception)
            {
                return string.Empty;
            }
        }
    }
}
