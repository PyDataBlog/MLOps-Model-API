using System.Collections.Generic;
using System.Linq;
using Ddd.Validation.Interfaces;

namespace Ddd.Validation.Common
{
    /// <inheritdoc/>
    public class ValidationResult : IValidationResult
    {
        private readonly List<IValidationError> _erros;

        private string Message { get; set; }

        /// <inheritdoc/>
        public bool IsValid => !_erros.Any();

        /// <inheritdoc/>
        public ICollection<IValidationError> Errors => _erros;

        /// <summary>
        /// Initializes a new instance of <see cref="ValidationResult"/>
        /// </summary>
        public ValidationResult()
        {
            _erros = new List<IValidationError>();
        }

        /// <inheritdoc/>
        public IValidationResult Add(string errorMessage)
        {
            _erros.Add(new ValidationError(errorMessage));
            return this;
        }

        /// <inheritdoc/>
        public IValidationResult Add(IValidationError error)
        {
            _erros.Add(error);
            return this;
        }

        /// <inheritdoc/>
        public IValidationResult Add(params IValidationResult[] validationResults)
        {
            if (validationResults == null) return this;

            foreach (var result in validationResults.Where(r => r != null))
                _erros.AddRange(result.Errors);

            return this;
        }

        /// <inheritdoc/>
        public IValidationResult Remove(IValidationError error)
        {
            if (_erros.Contains(error))
                _erros.Remove(error);
            return this;
        }
    }
}