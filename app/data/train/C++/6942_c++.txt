// Do NEVER include directly this file, it is already included by assert.hpp!

#ifndef _BOGEYMAN_PRINT_ERROR_LINE
# define _BOGEYMAN_PRINT_ERROR_LINE		BOGEYMAN_DEFAULT_PRINT_ERROR_LINE
#endif // !_BOGEYMAN_PRINT_ERROR_LINE_TO_OUTPUT

#if _BOGEYMAN_PRINT_ERROR_LINE == BOGEYMAN_PRINT_ERROR_LINE_TO_OUTPUT
# include <iostream>
# define BOGEYMAN_PRINT_ERROR_LINE()	std::cerr << "Assertion error: " << __FILE__ << " line " << __LINE__ << std::endl
#elif _BOGEYMAN_PRINT_ERROR_LINE == BOGEYMAN_LOG_ERROR_LINE_TO_FILE
# include "bogeyman/Logger.hpp"
# define BOGEYMAN_PRINT_ERROR_LINE()	bogeyman::Logger::getInstance().logErrorLine(__FILE__, __LINE__)
#elif _BOGEYMAN_PRINT_ERROR_LINE == BOGEYMAN_DONT_PRINT_ERROR_LINE
# define BOGEYMAN_PRINT_ERROR_LINE()
#else // !_BOGEYMAN_PRINT_ERROR_LINE_TO_OUTPUT
# error "Unrecognized value in parameter macro _BOGEYMAN_PRINT_ERROR_LINE_TO_OUTPUT."
#endif // !_BOGEYMAN_PRINT_ERROR_LINE_TO_OUTPUT
