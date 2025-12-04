use crate::{
    HostState, Module, VM, Value, compile_module,
    compiler::{self, report_error},
    vm::VMError,
};
use bevy::{
    asset::{AssetLoader, LoadContext, io::Reader},
    prelude::*,
};
use core::str;
use std::sync::{Arc, Mutex};
use thiserror::Error;

#[derive(TypePath, Asset)]
pub struct ModuleAsset {
    module: Module,
}

impl ModuleAsset {
    pub fn call<Params, Results>(
        &self,
        vm: &mut VM,
        func_name: &str,
        _args: &[Value],
    ) -> Result<Value, VMError> {
        vm.run(&self.module, func_name)
    }
}

#[non_exhaustive]
#[derive(Debug, Error)]
pub enum ModuleLoaderError {
    #[error("Could not load exemplar: {0}")]
    Io(#[from] std::io::Error),
    #[error("Could not decode script source from UTF-8: {0}")]
    DecodeUtf8(#[from] core::str::Utf8Error),
    // #[error("Invalid asset path: {0}")]
    // ParseAssetPath(#[from] bevy::asset::ParseAssetPathError),
    // #[error("Unable to read asset bytes: {0}")]
    // ReadAssetBytesError(#[from] bevy::asset::ReadAssetBytesError),
    #[error("{0}")]
    Compilation(#[from] compiler::CompilationError),
}

pub struct ModuleLoader {
    host: Arc<Mutex<HostState>>,
}

impl AssetLoader for ModuleLoader {
    type Asset = ModuleAsset;
    type Error = ModuleLoaderError;
    type Settings = ();

    async fn load(
        &self,
        reader: &mut dyn Reader,
        _settings: &Self::Settings,
        load_context: &mut LoadContext<'_>,
    ) -> Result<Self::Asset, Self::Error> {
        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes).await?;
        let path = load_context.path().to_str().unwrap().to_string();
        let src = str::from_utf8(&bytes)?;
        let host_ref = self.host.clone();

        match compile_module(&path, src, &host_ref).await {
            Ok(module) => Ok(ModuleAsset { module }),
            Err(err) => {
                report_error(&path, src, &err);
                Err(ModuleLoaderError::Compilation(err))
            }
        }

        // for import in unit.decls.imports.iter() {
        //     let import_path_str = unit.decls.symbols.resolve(import.path);
        //     let import_path = Path::new(&import_path_str).with_extension("saga");
        //     let path = load_context
        //         .asset_path()
        //         .resolve_embed(import_path.to_str().unwrap())?;
        //     println!("Loading import: {}", path);
        //     // let import_bytes = load_context.read_asset_bytes(path).await?;
        //     // load_context.get_handle(import).await?;
        // }
    }

    fn extensions(&self) -> &[&str] {
        &["fmod"]
    }
}
