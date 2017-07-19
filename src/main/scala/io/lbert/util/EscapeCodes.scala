package io.lbert.util

import java.io.OutputStream

class EscapeCodes(out: OutputStream) {

  // Output an Escape Sequence
  private def ESC(command: Char) { out.write(s"\033$command".getBytes) }
  // Output a Control Sequence Inroducer
  private def CSI(sequence: String) { out.write(s"\033[$sequence".getBytes) }
  // Execute commands
  private def CSI(command: Char) { CSI(s"$command") }
  private def CSI(n: Int, command: Char) { CSI(s"$n$command") }
  private def CSI(n: Int, m: Int, command: Char) { CSI(s"$n;$m$command") }
  private def CSI(n: Int, m: Int, o: Int, command: Char) { CSI(s"$n;$m;$o$command") }
  // Execute commands in private modes
  private def CSI(mode: Char, command: Char) { CSI(s"$mode$command") }
  private def CSI(mode: Char, n: Int, command: Char) { CSI(s"$mode$n;$command") }

  /* DSR */ def status() { CSI(5, 'n') }

  // Cursor movement
  /* CUU */ def moveUp   (n: Int = 1) { CSI(n, 'A') }
  /* CUD */ def moveDown (n: Int = 1) { CSI(n, 'B') }
  /* CUF */ def moveRight(n: Int = 1) { CSI(n, 'C') }
  /* CUB */ def moveLeft (n: Int = 1) { CSI(n, 'D') }
  /* CUP */ def move(y: Int, x: Int)  { CSI(x + 1, y + 1, 'H') }

  // Cursor management
  /* DECTCEM */ def hideCursor()    { CSI('?', 25, 'l') }
  /* DECTCEM */ def showCursor()    { CSI('?', 25, 'h') }
  /*  DECSC  */ def saveCursor()    { ESC('7') }
  /*  DECRC  */ def restoreCursor() { ESC('8') }

  // Screen management
  /*   ED   */ def clear()                      { CSI(2, 'J') }
  /* DECSET */ def alternateBuffer()            { CSI('?', 47, 'h') }
  /* DECRST */ def normalBuffer()               { CSI('?', 47, 'l') }
  /*   RIS  */ def fullReset()                  { ESC('c') }
  /* dtterm */ def resizeScreen(w: Int, h: Int) { CSI(8, w, h, 't') }

  // Window management
  /* dtterm */ def unminimizeWindow()           { CSI(1, 't')}
  /* dtterm */ def minimizeWindow()             { CSI(2, 't')}
  /* dtterm */ def moveWindow(x: Int, y: Int)   { CSI(3, x, y, 't') }
  /* dtterm */ def resizeWindow(w: Int, h: Int) { CSI(4, w, h, 't') }
  /* dtterm */ def moveToTop()                  { CSI(5, 't') }
  /* dtterm */ def moveToBottom()               { CSI(6, 't') }
  /* dtterm */ def restoreWindow()              { CSI(9, 0, 't')}
  /* dtterm */ def maximizeWindow()             { CSI(9, 1, 't')}

  // Color management
  /* ISO-8613-3 */ def setForeground(color: Int) { CSI(38, 5, color, 'm') }
  /* ISO-8613-3 */ def setBackground(color: Int) { CSI(48, 5, color, 'm') }
  /*     SGR    */ def startBold()      { CSI(1, 'm') }
  /*     SGR    */ def startUnderline() { CSI(4, 'm') }
  /*     SGR    */ def startBlink()     { CSI(5, 'm') }
  /*     SGR    */ def startReverse()   { CSI(7, 'm') }
  /*     SGR    */ def stopBold()       { CSI(22, 'm') }
  /*     SGR    */ def stopUnderline()  { CSI(24, 'm') }
  /*     SGR    */ def stopBlink()      { CSI(25, 'm') }
  /*     SGR    */ def stopReverse()    { CSI(27, 'm') }
  /*     SGR    */ def stopForeground() { CSI(39, 'm')}
  /*     SGR    */ def stopBackground() { CSI(49, 'm')}
  /*     SGR    */ def resetColors()    { CSI(0, 'm') }


}
