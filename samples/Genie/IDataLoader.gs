/**
 * IDataLoader.gs
 *
 * 
 *
 */
[indent=4]
uses Gee
uses sdx
uses sdx.math
uses sdx.graphics.s2d
uses o2d.data

namespace o2d.resources

    interface IDataLoader : Object
        def abstract loadSceneVO(sceneName: string): SceneVO
        def abstract loadProjectVO(): ProjectInfoVO
