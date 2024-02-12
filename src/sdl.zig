pub usingnamespace @cImport({
    @cInclude("SDL2/SDL.h");
    @cInclude("SDL2/SDL_syswm.h");
});

// When using system sdl2 library this might not be defined
pub const SDL_HINT_WINDOWS_DPI_AWARENESS_ = "SDL_WINDOWS_DPI_AWARENESS";
pub const SDL_HINT_WINDOWS_DPI_SCALING_ = "SDL_WINDOWS_DPI_SCALING";
