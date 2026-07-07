import 'dart:ffi';
import 'dart:io';
import 'dart:typed_data';
import 'dart:isolate';
import 'dart:async';

import 'package:ffi/ffi.dart';

// TODO: support more platforms
DynamicLibrary _loadNesLib() {
  if (Platform.isLinux) {
    return DynamicLibrary.executable();
  } else if (Platform.isAndroid) {
    return DynamicLibrary.open("libnes.so");
  }

  throw UnsupportedError('Unsupported platform: ${Platform.operatingSystem}');
}

final class InputState extends Struct {
  @Bool()
  external bool a;
  @Bool()
  external bool b;
  @Bool()
  external bool select;
  @Bool()
  external bool start;
  @Bool()
  external bool up;
  @Bool()
  external bool down;
  @Bool()
  external bool left;
  @Bool()
  external bool right;
}

// bool nes_init(uint8_t *data, size_t len);
typedef _NesInitNative = Bool Function(Pointer<Uint8> data, IntPtr len);
typedef _NesInitDart = bool Function(Pointer<Uint8> data, int len);

// bool nes_is_initialized(void);
typedef _NesIsInitializedNative = Bool Function();
typedef _NesIsInitializedDart = bool Function();

// void nes_frame(void);
typedef _NesFrameNative = Void Function();
typedef _NesFrameDart = void Function();

// uint8_t *nes_frame_buffer(void);
typedef _NesFrameBufferNative = Pointer<Uint8> Function();
typedef _NesFrameBufferDart = Pointer<Uint8> Function();

// void nes_set_input_controller_a(InputState state);
typedef _NesSetInputANative = Void Function(InputState state);
typedef _NesSetInputADart = void Function(InputState state);

// void nes_set_input_controller_b(InputState state);
typedef _NesSetInputBNative = Void Function(InputState state);
typedef _NesSetInputBDart = void Function(InputState state);

class LibNes {
  LibNes._internal() {
    _lib = _loadNesLib();
    _nesInit = _lib.lookupFunction<_NesInitNative, _NesInitDart>('nes_init');
    _nesIsInitialized = _lib
        .lookupFunction<_NesIsInitializedNative, _NesIsInitializedDart>(
          'nes_is_initialized',
        );
    _nesFrame = _lib.lookupFunction<_NesFrameNative, _NesFrameDart>(
      'nes_frame',
    );
    _nesFrameBuffer = _lib
        .lookupFunction<_NesFrameBufferNative, _NesFrameBufferDart>(
          'nes_frame_buffer',
        );
    _nesSetInputA = _lib.lookupFunction<_NesSetInputANative, _NesSetInputADart>(
      'nes_set_input_controller_a',
    );
    _nesSetInputB = _lib.lookupFunction<_NesSetInputBNative, _NesSetInputBDart>(
      'nes_set_input_controller_b',
    );

    print(_lib.toString());
  }

  static final LibNes _instance = LibNes._internal();
  factory LibNes() => _instance;

  late final DynamicLibrary _lib;
  late final _NesInitDart _nesInit;
  late final _NesIsInitializedDart _nesIsInitialized;
  late final _NesFrameDart _nesFrame;
  late final _NesFrameBufferDart _nesFrameBuffer;
  late final _NesSetInputADart _nesSetInputA;
  late final _NesSetInputBDart _nesSetInputB;

  static const int frameBufferSize = 256 * 240 * 4;

  Pointer<Uint8>? _romBuffer;

  // load a rom, copies rom data into its memory
  // own and keeps rom data alive until dispose is called
  bool nesInit(Uint8List romBytes) {
    _freeRomBuffer();

    final buffer = malloc.allocate<Uint8>(romBytes.length);
    buffer.asTypedList(romBytes.length).setAll(0, romBytes);
    _romBuffer = buffer;

    return _nesInit(buffer, romBytes.length);
  }

  bool nesIsInitialized() => _nesIsInitialized();

  void nesFrame() => _nesFrame();

  // Returns a view over the native frame buffer
  // This does not copy the underlaying data
  Uint8List nesFrameBuffer() {
    final ptr = _nesFrameBuffer();
    return ptr.asTypedList(frameBufferSize);
  }

  void nesSetInputControllerA({
    bool a = false,
    bool b = false,
    bool select = false,
    bool start = false,
    bool up = false,
    bool down = false,
    bool left = false,
    bool right = false,
  }) {
    final state = malloc<InputState>();
    state.ref
      ..a = a
      ..b = b
      ..select = select
      ..start = start
      ..up = up
      ..down = down
      ..left = left
      ..right = right;
    _nesSetInputA(state.ref);
    malloc.free(state);
  }

  void nesSetInputControllerB({
    bool a = false,
    bool b = false,
    bool select = false,
    bool start = false,
    bool up = false,
    bool down = false,
    bool left = false,
    bool right = false,
  }) {
    final state = malloc<InputState>();
    state.ref
      ..a = a
      ..b = b
      ..select = select
      ..start = start
      ..up = up
      ..down = down
      ..left = left
      ..right = right;
    _nesSetInputB(state.ref);
    malloc.free(state);
  }

  void _freeRomBuffer() {
    if (_romBuffer != null) {
      malloc.free(_romBuffer!);
      _romBuffer = null;
    }
  }

  void dispose() {
    _freeRomBuffer();
  }
}
