#pragma once

#include "cinder/app/App.h"

namespace reza { namespace paths {
    
bool createDirectory( const ci::fs::path& path );
bool createDirectories( const ci::fs::path& path );
void createAssetDirectories();
ci::fs::path getWorkingPath();
ci::fs::path getPath( std::string path = "" );
ci::fs::path getPresetsPath( std::string path = "" );
ci::fs::path getDataPath( std::string path = "" );
ci::fs::path getAudioPath( std::string path = "" );
ci::fs::path getVideoPath( std::string path = "" );
ci::fs::path getFontsPath( std::string path = "" );
ci::fs::path getModelsPath( std::string path = "" );
ci::fs::path getImagesPath( std::string path = "" );
ci::fs::path getMatCapsPath( std::string path = "" );
ci::fs::path getPalettesPath( std::string path = "" );
ci::fs::path getRendersPath( std::string path = "" );
ci::fs::path getShadersPath( std::string path = "" );
    
} } // namespace reza::paths