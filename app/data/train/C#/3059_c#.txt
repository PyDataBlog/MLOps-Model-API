using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using OlympicGames.Core.Commands.Abstracts;
using OlympicGames.Core.Contracts;
using OlympicGames.Core.Factories;
using OlympicGames.Olympics.Contracts;
using OlympicGames.Utils;

namespace OlympicGames.Core.Commands
{
    public class CreateSprinterCommand : Command, ICommand
    {
        private IDictionary<string, double> records = new Dictionary<string, double>();
        private string command;

        public CreateSprinterCommand(IList<string> commandParameters)
            : base(commandParameters)
        {
            commandParameters.ValidateIfNull();
            this.command = string.Join(" ", CommandParameters);
        }

        public override string Execute()
        {
            var commandTokens = command
                .Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)
                .ToArray();

            var firstName = commandTokens[0];
            firstName.ValidateMinAndMaxLength(2, 20);
            var lastName = commandTokens[1];
            lastName.ValidateMinAndMaxLength(2, 20);
            var country = commandTokens[2];
            country.ValidateMinAndMaxLength(3, 25);

            this.records = new Dictionary<string, double>();
            foreach (var kvp in commandTokens.Skip(3).ToArray())
            {
                var kvpTokens = kvp.Split('/').ToArray();
                records.Add(kvpTokens[0], double.Parse(kvpTokens[1]));
            }

            Committee.Olympians.Add(OlympicsFactory.Instance.CreateSprinter(firstName, lastName, country, records));

            var commandOutput = new StringBuilder();
            commandOutput.AppendLine("Created Sprinter");
            commandOutput.AppendLine($"SPRINTER: {firstName} {lastName} from {country}");
            commandOutput.AppendLine($"PERSONAL RECORDS:");
            foreach (var kvp in records)
            {
                commandOutput.AppendLine($"{kvp.Key}m: {kvp.Value}s");
            }

            return commandOutput.ToString();
        }
    }
}
