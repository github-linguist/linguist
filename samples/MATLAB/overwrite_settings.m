function settings = overwrite_settings(defaultSettings, overrideSettings)
% function settings = overwrite_settings(defaultSettings, overrideSettings)
% Returns the settings based on a combination of the defaults and the override settings.
%
% Parameters
% ----------
% defaultSettings : structure
%   A structure with all of the default settings.
% overrideSettings : structure
%   Contains any settings that should override the defaults.
%
% Returns
% -------
% settings : structure
%   A stucture containing the overrideSettings and any defaults that weren't
%   supplied in the overrideSettings.

% add the default options if not specified by the user
overrideNames = fieldnames(overrideSettings);
defaultNames = fieldnames(defaultSettings);
notGiven = setxor(overrideNames, defaultNames);
settings = overrideSettings;
if length(notGiven) > 0
    for i = 1:length(notGiven)
        settings.(notGiven{i}) = defaultSettings.(notGiven{i});
    end
end
