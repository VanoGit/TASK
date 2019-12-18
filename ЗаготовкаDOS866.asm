; ���⮩ �����஢騪 �����
; ~~~~~~~~~~~~~~~~~~~~~~~~~
; �ந������ ��४��祭�� � ���饭�� ०�� � ���樠����樥�
; ⠡��� GDT � IDT � ᥣ���⮢ TSS. �����⢫�� ��४��祭�� ����� �६�
; 䨪�஢���묨 ����砬� � ��㣮��� ���浪�, ��४��祭�� �ந�室�� ��
; ���祭�� ����� �६��� (�� ���뢠��� �� ⠩���), �.�. ॠ�������
; ��堭��� ������饩 ���������筮��. ��ࠡ��뢠���� �� ���뢠��� �
; �᪫�祭�� (��� ⮣� �⮡� ���-� ���뢠��� �� ������뢠���� �� �᪫�祭��,
; 1� ��� ��९ணࠬ������� ��� �⮡ࠦ���� Irq0 �� Int 20h � �.�.).
; �� �᪫�祭�� �ਢ���� � �뤠� ᮮ�饭�� � �����襭�� �ணࠬ��,
; ����⨥ Esc - ��室 �� �ணࠬ��.
;
; ��������� TASM:
;       tasm /zi <��� 䠩��>.asm
;       tlink /3 /v <��� 䠩��>.obj
; ��������� WASM:
;       wasm /D <��� 䠩��>.asm
;       wlink debug file <��� 䠩��>.obj form DOS
;
so     equ     offset  ; ��� WASM
;so      equ     small offset    ; ��� TASM
PAUSE   EQU     700000H ; ����প� �� ����ᮢ�� ᨬ����
.386p   ; ����襭� �� ������� I386



; ������� ������ ��� ���饭���� ०���, ᮤ�ন� GDT, IDT � ᥣ����� TSS
PM_data SEGMENT para public 'DATA' use32

; ⠡��� ��������� ���ਯ�஢ 
GDT     LABEL   byte
; �㫥��� ���ਯ�� (��易⥫쭮 ������ ���� �� ��ࢮ� ����)
db      8 dup(0)
; 16-���� 64-��������� ᥣ���� ���� � ����� RM_seg
GDT_16bitCS     db      0FFh,0FFh,0,0,0,10011000b,0,0
; 16-���� 64-��������� ᥣ���� ������ � �㫥��� �����
GDT_R_MODE_DATA db      0FFh,0FFh,0,0,0,10010010b,0,0
; 32-���� 64-��������� ᥣ���� ������ � ����� 0B8000h
GDT_VideoBuf    db      0FFh,0FFh,0,80h,0Bh,11110010b,01000000b,0
; 32-���� 4-��������� ᥣ���� ���� � ����� PM_code
GDT_32bitCS     db      0FFh,0FFh,0,0,0,10011010b,11001111b,0
; 32-���� 4-��������� ᥣ���� ������ � ����� PM_data
GDT_32bitDS     db      0FFh,0FFh,0,0,0,10010010b,11001111b,0
; 32-���� 4-��������� ᥣ���� ������ � ����� Stak_seg
GDT_32bitSS     db      0FFh,0FFh,0,0,0,10010010b,11001111b,0
; 32-���� ᢮����� TSS ����� 0 � ����⮬ 67h
GDT_TSS0        db      67h,0,0,0,0,10001001b,01000000b,0
; 32-���� ᢮����� TSS ����� 1 � ����⮬ 67h
GDT_TSS1        db      67h,0,0,0,0,10001001b,01000000b,0
; 32-���� ᢮����� TSS ����� 2 � ����⮬ 67h
GDT_TSS2        db      67h,0,0,0,0,11101001b,01000000b,0
; 32-���� ��࠭�祭�� ᥣ���� ���� ����� 2
GDT_Task2_CS	db		0,0,0,0,0,11111010b,01000000b,0
; 32-���� ��࠭�祭�� ᥣ���� ������ ����� 2
GDT_Task2_DS	db		0,0,0,0,0,11110010b,01000000b,0
; 32-���� ��࠭�祭�� ᥣ����� �⥪� ����� 2
GDT_Task2_SS3	db		0FFh,0,0,0,0,11110010b,01000000b,0
GDT_Task2_SS0	db		0FFh,0,0,0,0,10010010b,01000000b,0
; 32-���� ��࠭�祭�� ᥣ���� �����쭮� ⠡���� ���ਯ�஢ ����� 2
GDT_Task2_LDT	db		0,0,0,0,0,11100010b,01000000b,0
gdt_size = $-GDT
GDTr    dw      gdt_size-1      ; ����� GDT
dd      ?       ; ����� �㤥� 32-���� ������� ���� GDT


; ⠡��� �������� ���ਯ�஢ ����� 2
LDT2     LABEL   byte
; �㫥��� ���ਯ�� (��易⥫쭮 ������ ���� �� ��ࢮ� ����)
db      8 dup(0)
; 32-���� ��࠭�祭�� ᥣ���� ������ ����� 2
LDT2_DS	db		0,0,0,0,0,11110010b,01000000b,0
ldt2_size = $-LDT2
LDT2_Limit    	dw      ldt2_size-1      ; ����� LDT2


; ����� ��� ᥫ���஢ (�� ᥫ����� ��� GDT, � RPL = 00)
SEL_16bitCS     equ     0001000b
SEL_R_MODE_DATA equ     0010000b
SEL_VideoBuf    equ     0011000b
SEL_32bitCS     equ     0100000b
SEL_32bitDS     equ     0101000b
SEL_32bitSS     equ     0110000b
SEL_TSS0        equ     0111000b
SEL_TSS1        equ     1000000b
SEL_TSS2 		equ     1001011b
SEL_Task2_CS	equ		1010011b
SEL_Task2_DS	equ		1011011b
SEL_Task2_SS3	equ		1100011b
SEL_Task2_SS0	equ		1101000b
SEL_Task2_LDT	equ 	1110011b	; ᥫ���� �����쭮� ⠡���� ����� 2 � ������쭮� ⠡���

SEL_LDT2_DS		equ		0001111b	; ᥫ���� ᥣ���� ������ ����� 2 � �����쭮� ⠡��� ����� 2


; ⠡��� ���ਯ�஢ ���뢠���
IDT     LABEL   byte
; INT 00h - 1Fh (�᪫�祭��)
; �� �� ���ਯ��� ����� ⨯ 0Fh -> 32-���� �� ����誨
dw      32 dup (so Exept_h,SEL_32bitCS,8F00h,0)
; �� ᫥�. ���ਯ��� ����� ⨯ 0Eh -> 32-���� �� ���뢠���
; INT 20h (Irq0)
dw      so Plan,SEL_32bitCS,8E00h,0
; INT 21h (Irq1)
dw      so Irq1_h,SEL_32bitCS,8E00h,0
; INT 22h (Irq2)
dw      so Irq2_7_h,SEL_32bitCS,8E00h,0
; INT 23h - 24h (Irq3 - Irq4 COM2,COM1)
dw      2 dup (so Receive,SEL_32bitCS,8E00h,0)
; INT 25h - 27h (Irq5 - Irq7)
dw      3 dup (so Irq2_7_h,SEL_32bitCS,8E00h,0)
; INT 28h - 6Fh
dw      72 dup (so Int_h,SEL_32bitCS,8E00h,0)
; INT 70h - 77h (Irq8 - Irq15)
dw      8 dup (so Irq8_15_h,SEL_32bitCS,8E00h,0)
; INT 78h - FFh
dw      136 dup (so Int_h,SEL_32bitCS,8E00h,0)
idt_size = $-IDT
IDTr    dw      idt_size-1      ; ����� IDT
dd      ?       ; ����� �㤥� 32-���� ������� ���� IDT

; ᮤ�ন��� ॣ���� IDTR � ॠ�쭮� ०���
IDTr_real dw    3FFh,0,0

; ᥣ���� TSS_0 ����� 0 �㤥� ���樠����஢�� � ��砫� ����� 0, ����� ��筥�
; �믮������� �ࠧ� ��᫥ ��४��祭�� �� � ���饭�� ०��. ����筮, 
; �᫨ �� �� ᮡ�ࠫ��� �ᯮ�짮���� ��᪮�쪮 �஢��� �ਢ������, � 
; �㦭� �뫮 �� ���樠����஢��� �⥪�.
TSS_0   db      68h dup(0)

; ᥣ���� TSS_1 ����� 1. � ��� �㤥� �믮������� ��४��祭�� �������� jmp ��
; �����஢騪� �����, ⠪ �� ���� ���樠����஢��� ��, �� ����� ���ॡ�������:
TSS_1   dd      8 dup(0)        ; ���, �⥪�, CR3
dd      offset Task_1   ;       EIP
dd      0200h   ;       EFLAGS (IF=1, DF=0)
dd      0Ah*256 ;       Symb_Col*256    ;       EAX
dd      0,0,0   ;       ECX, EDX, EBX
dd      Stack_1 ;       ESP
dd      0,0     ;       EBP, ESI
dd      LastPos_1       ;       EDI (��� �뢮�� ��砫쭮�� ᨬ����)
dd      SEL_VideoBuf    ;       ES
dd      SEL_32bitCS     ;       CS
dd      SEL_32bitSS     ;       SS
dd      SEL_32bitDS     ;       DS
dd      0,0     ;       FS, GS
dd      0       ;       LDTR
dw      0       ;       ᫮�� 䫠��� �����
dw      0       ;       ���� ⠡���� �����-�뢮��

; ᥣ���� TSS_2. �������筮 TSS_1:
TSS_2   dw		0		; ᥫ���� TSS ���⭮� �裡 (��� ��������� �����)
dw		0
dd		Stack_2_PL0		; ESP ��� �⥪� �஢�� �ਢ������ 0
dw		SEL_Task2_SS0	; SS ��� �⥪� �஢�� �ਢ������ 0 (������ ���� RPL = 00)
dw		0
dd		0				; ESP ��� �⥪� �஢�� �ਢ������ 1
dw		0				; SS ��� �⥪� �஢�� �ਢ������ 1 (������ ���� RPL = 01)
dw		0
dd		0				; ESP ��� �⥪� �஢�� �ਢ������ 2
dw		0				; SS ��� �⥪� �஢�� �ਢ������ 2 (������ ���� RPL = 10)
dw		0
dd      0        		; CR3, ���� ��⠫��� ⠡��� ��࠭�� (�� ��࠭�筮� ����樨)
dd      offset Task_2   ; EIP
dd      0200h           ; EFLAGS (IF=1, DF=0)
dd      05h*256    		; EAX
dd      0,0,0           ; ECX, EDX, EBX
dd      Stack_2_PL3     ; ESP ��� �⥪� �஢�� �ਢ������ ᥣ���� ���� �����, � ������ ��砥 3
dd      0,0             ; EBP, ESI
dd      LastPos_2       ; EDI (��� �뢮�� ��砫쭮�� ᨬ����)
dd      SEL_VideoBuf    ; ES
dd      SEL_Task2_CS    ; CS
dd      SEL_Task2_SS3   ; SS ��� �⥪� �஢�� �ਢ������ ᥣ���� ���� �����, � ������ ��砥 3
dd      SEL_Task2_DS    ; DS
dd      0,0             ; FS, GS
dw      SEL_Task2_LDT   ; LDTR
dw		0
dw      0               ; ᫮�� 䫠��� �����
dw      0               ; ���� ⠡���� �����-�뢮��

; ���稪 (��� �����஢騪�)
Counter dw      0
; [�������]:[���饭��] ��� ��� TSS ��� ���쭥�� jmp'� (��� �����஢騪�)
Sel_0   dd      0       ; ᬥ饭��
dw      SEL_TSS0        ; ᥫ����
dw      ?
Sel_1   dd      0       ; ᬥ饭��
dw      SEL_TSS1        ; ᥫ����
dw      ?
Sel_2   dd      0       ; ᬥ饭��
dw      SEL_TSS2        ; ᥫ����

; ����饭�� �� �᪫�祭��
Exp_msg LABEL   byte
IRPC chr, <!!! Exception raised - program exits !!!> 
  db '&chr&', 0Ch
ENDM
Emsg_size = $ - Exp_msg

; ����⠭��, ����� �ᯮ������� � ������
LastPos_0       =       027Eh
LastPos_1       =       075Eh
LastPos_2       =       0C7Eh
Delta   =       9Ch
Symb_Col        EQU       0Ah
Symb_Div        =       20h 
DataBufer    db 40 dup (30h)
;���� ������
COUNT   DB      20h
SMVL    DB      20h
;��६���
COM     dw      0
IER     dw      0
IIR     dw      0
LCR     dw      0
MCR     dw      0
LSR     dw      0
MSR     dw      0

PM_data ENDS



; 32-���� ᥣ����, ᮤ�ঠ騩 ���, ����� �㤥� �ᯮ������� � ���饭��� ०��� 
PM_code SEGMENT para public 'CODE' use32
ASSUME  cs: PM_code
ASSUME  ds: PM_data

; �窠 �室� � 32-���� ���饭�� ०��
PM_ENTRY:
;����� �㤥� ���樠����஢��� �����奬� ����஫��� ���뢠���
cli     ;����頥�
MOV     AL,0
MOV     DX,MCR
OUT     DX,AL
JMP     $+2
MOV     DX,LSR
IN      AL,DX
JMP     $+2
MOV     DX,MSR
IN      AL,DX
JMP     $+2
MOV     DX,COM
IN      AL,DX
JMP     $+2
; ���樠������ COM ����
MOV     AX, 2F8H ;��� ������ ���� ���� ������� COM2
MOV     COM, AX ; ��� ���� 2f8
INC     AX
MOV     IER, AX ; ���� ॣ���� ࠧ�襭�� ���뢠���
INC     AX
MOV     IIR, AX ; ���� ॣ���� �����䨪�樨 ���뢠���
INC     AX
MOV     LCR, AX ; ���� ॣ���� �ࠢ����� ������
INC     AX
MOV     MCR, AX ; ���� ॣ���� �ࠢ����� �������
INC     AX
MOV     LSR, AX ; ���� ॣ���� ���ﭨ� �����
INC     AX
MOV     MSR,AX ; ���� ॣ���� ���ﭨ� ������
XOR     AX,AX
; yc⠭���� speeda
MOV     DX,LCR
MOV     AL,10000000B
OUT     DX,AL
MOV     DX,COM
MOV     AL,60H
OUT     DX,AL
INC     DX
MOV     AL,0
OUT     DX,AL
;���樠��� ॣ���� ����஫� �����
MOV     DX,LCR
MOV     AL,00H
OR      AL,00000011B
OR      AL,00000000B
OR      AL,00000000B
OR      AL,00000000B
OUT     DX,AL
;�ࠢ����� ॣ���� �ࠢ����� �������
MOV     DX,MCR
MOV     AL,00011000B ; ������ ���� 00011000B (�⮡� ࠧ���� ���뢠��� �� COM-����)
OUT     DX,AL
;mcr ready
MOV     DX,IER
MOV     AL,00000111B
OUT     DX,AL
; ����஫��� ���뢠���
IN      AL,21H
AND     AL,11100111B
OUT     21H,AL
MOV     AL,20H
OUT     20H,AL
OUT     0A0H,AL
STI     ;ࠧ�蠥� ���뢠���
;䨭��
; �����⮢��� ॣ����� ����� 0
mov     ax, SEL_32bitDS
mov     ds, ax
mov     ax, SEL_VideoBuf
mov     es, ax
mov     ax, SEL_32bitSS
mov     ss, ax
mov     esp, Stack_0
; ����㧨�� TSS ����� 0 � ॣ���� TR
mov     ax, SEL_TSS0
ltr     ax
; �����⮢��� ॣ�����
mov     edi, LastPos_0
mov     ah, Symb_Col
cld
; ࠧ���� ���뢠���
sti

; ����� 0
Task_0:
sub     edi, 2
mov     ah,02h                     ;��ᨬ
mov     al,SMVL
dec     al
cmp     al,1Fh                     ;��� 1f �뢮���� �� �㤥�
jnz     @1
mov     al,20h
@1:     stosw
cmp     edi, LastPos_0
jnz     short @0
sub     edi, Delta
@0:     mov     al, SMVL        ;Symb_Code
mov     ah, 1Eh ;��⨢�� 梥�
stosw
; ���� ���ᠭ ��।��稪
MOV     DX,LSR
SNDRDY: IN      AL,DX
TEST    AL,00100000B
JZ      SNDRDY
MOV     AL,SMVL
CMP     AL,0FFH
JNZ     N_RESET
MOV     AL,20H
N_RESET:
MOV     DX,COM
OUT     DX,AL
;��࠭�� ᫥���騩 ᨬ���
INC     AL
MOV     SMVL,AL
; �������� ��㧠, �������� �� ᪮��� ������
mov     ecx, Pause
;mov     ecx, 700000h
loop    $
jmp     short Task_0


; ����� 1
Task_1:
mov     cx,28h  ;length
mov     esi,0   ;offcet
sub     edi,54h ;correct edi
next:
mov     al,[Databufer+esi]
inc     esi
stosw
loop    next
mov     al,20h  ; �뢥�� �஡��
stosw
mov     al,Count        ; � �� ��� ������窠
cmp     al,20h
jnz     toreset
mov     al,2Ah
jmp     print
toreset:
mov     al,20h
print:
mov     Count,al
mov     ah,0Ch
stosw
mov     ah,0Ah  ;����⠭�������� 梥�
mov     ecx, Pause
;mov     ecx, 700000h
loop    $
jmp     short Task_1


; ��ࠡ��稪 �᪫�祭��
Exept_h: cli
mov bx, SEL_32bitDS
mov ds, bx
mov bx, SEL_VideoBuf
mov es, bx

; ���⠥� ��� �訡�� � �⥪�
;pop ecx
;and ecx, 0100b
;jnz Skip
; �뢮��� ᮮ�饭��
mov     esi, offset Exp_msg
mov     ecx, Emsg_size
cld
rep     movsb
Skip:
; � ��室�� � ॠ��� ०��
db      0EAh    ; ��� ���쭥�� jmp
dd      offset RM_return        ; 32-��⭮� ᬥ饭�� RM_return
dw      SEL_16bitCS     ; ᥫ���� RM_seg


; ��ࠡ��稪 �ணࠬ����� ���뢠���
Int_h:
iretd


; ��ࠡ��稪 �����⭮�� ���뢠��� Irq2 - Irq7
Irq2_7_h:
push    eax
mov     al, 20h
out     20h, al         ; ��᫠�� EOI ����஫���� ���뢠��� #1
pop     eax
iretd


; ��ࠡ��稪 �����⭮�� ���뢠��� Irq8 - Irq15
Irq8_15_h:
push    eax
mov     al, 20h
out     0A0h, al        ; ��᫠�� EOI ����஫���� ���뢠��� #2
pop     eax
iretd


; ��ࠡ��稪 Irq0 - ���뢠��� �� ⠩��� (ᠬ �����஢騪)
Plan:
push    ds                      ; ��࠭塞 ॣ�����
push    ebx
push    eax
mov     ax, SEL_32bitDS
mov     ds, ax                  ; ᥫ���� PM_data -> � ds
xor     ebx, ebx
mov     bx, Counter             ; �⠥� Counter,
inc     ebx                     ; 㢥��稢��� ��� �� 1
cmp     ebx, 3                  ; � �஢��塞: �᫨ �� ᮮ�-��
jnz     short @OK               ; ����� 2, � ��⠭�������� ���
xor     ebx, ebx                ; �� ������ 0,
@OK:    mov     Counter, bx     ; ��࠭塞 Counter
mov     al, 20h
out     20h, al                 ; ���뫠�� EOI ����஫���� ���뢠���
jmp     fword ptr [ebx*8+Sel_0] ; ��४��砥��� �� ������ �Counter
pop     eax
pop     ebx                     ; ����⠭�������� ॣ����� � ��室��
pop     ds
iretd



; ��ࠡ��稪 IRQ3-IRQ4 - ���뢠��� �� COM-���⮢
RECEIVE:
PUSH    EAX
PUSH    ESI
PUSH    DX
MOV     ESI,39
movebufer:
MOV     AL,[DATABUFER+ESI-1]
MOV     [DATABUFER+ESI],AL
DEC     ESI
CMP     ESI,0
JNZ     MOVEBUFER
;⥯��� ࠡ�⠥� � ���⮬ (i8250)
MOV     DX,IIR  ;interrupt testing
IN      AL,DX
RCR     AL,1
JC      BYE
DATRDY: MOV     DX,LSR  ;data present testing
IN      AL,DX
TEST    AL,00001110B
JNZ     BYE     ;�� ᠬ�� ���� ��� ������ ����
;��릮� �� ��ࠡ��稪 �訡��
;TEST    AL,00000001B <-- ��� ��⮢���� ������ ��祬�-� �� ���⠢����� DosBox'��
;JZ      DATRDY       <-- ���⮬� ��諮�� ���������஢��� ��� �஢���.
MOV     DX,COM  ;receiving
IN      AL,DX
PUSH    AX 
BUFRDY: MOV     DX,LSR  ;data received, Buffer is empty
IN      AL,DX
TEST    AL,00000001B
JNZ     BUFRDY
POP     AX      ;��� �� ���� ������ �뫨 ������ �� �⥪�
MOV     [DATABUFER],AL  ;����ᠫ� ����� ���祭�� � ��砫� 
bye:    MOV     AL, 20H
OUT     20H,AL  ; ��᫠�� EOI ����஫���� ���뢠��� #1
POP     DX
POP     ESI
POP     EAX
IRETD


; ��ࠡ��稪 Irq1 - ���뢠��� �� ����������
Irq1_h:
push    eax
in      al, 60h         ; ������ ᪠�-��� ����⮩ ������,
cmp     al, 1           ; �᫨ �� Esc,
jz      Esc_pressed     ; ��� � ॠ��� ०��,
in      al, 61h         ; ����:
or      al, 80h
out     61h, al         ; ࠧ���� ࠡ��� ����������
mov     al, 20h
out     20h, al         ; ��᫠�� EOI ����஫���� ���뢠��� #1
pop     eax
iretd   ; � �������� ��ࠡ��稪

; � ��।����� �ࠢ����� �� ��ࠡ��稪� Irq1, �᫨ ����� Esc
Esc_pressed:
in      al, 61h
or      al, 80h
out     61h, al         ; ࠧ���� ࠡ��� ����������
mov     al, 20h
out     20h, al         ; ��᫠�� EOI ����஫���� ���뢠��� #1
cli     ; ������� ���뢠���
db      0EAh    ; � �������� � ॠ��� ०��
dd      offset RM_return
dw      SEL_16bitCS

PM_code ENDS



; ������� ������ ����� 2
Task2_DS SEGMENT PARA PUBLIC 'DATA' use32

Task2_DS_Start LABEL byte

MYVAR 	DB	5

Task2_DS_Size  EQU $-Task2_DS_Start
Task2_DS_Limit dw Task2_DS_Size - 1

Task2_DS ENDS



; ������� ���� ����� 2
Task2_CS SEGMENT PARA PUBLIC 'CODE' use32
ASSUME CS:Task2_CS
ASSUME DS:Task2_DS
ASSUME SS:Task2_SS3

Task2_CS_Start LABEL byte
; ����� 2
Task_2:
mov 	bx, SEL_LDT2_DS
mov		fs, bx

xor     al, al
sub     edi, 2
stosw
cmp     edi, LastPos_2
jnz     short @2
sub     edi, Delta
@2: mov al, fs:[MYVAR] ;Symb_Code access using LDT
stosw
; �������� ��㧠, �������� �� ᪮��� ������
mov     ecx, Pause
;mov     ecx, 700000h
loop    $
jmp     short Task_2
Task2_CS_Size  EQU $-Task2_CS_Start
Task2_CS_Limit dw Task2_CS_Size - 1

Task2_CS ENDS



; 32-���� ᥣ���� �⥪� ����� 2 ��� �஢�� �ਢ������ 3
Task2_SS3 SEGMENT para stack 'STACK' use32

Task2_SS3_End db      	100h dup(?)     ; �⥪ ����� 2 �஢�� �ਢ������ 3
Stack_2_PL3 = $-Task2_SS3_End

Task2_SS3 ENDS



; 32-���� ᥣ���� �⥪� ����� 2 ��� �஢�� �ਢ������ 0
Task2_SS0 SEGMENT para stack 'STACK' use32

Task2_SS0_End db		100h dup(?)     ; �⥪ ����� 2 �஢�� �ਢ������ 0
Stack_2_PL0 = $-Task2_SS0_End

Task2_SS0 ENDS



; ������� �⥪�. �ᯮ������ ��� 16-���� � 16-��⭮� ��� �ணࠬ�� � ���
; 32-���� (�१ ᥫ���� SEL_32bitSS) � 32-��⭮� ���.
Stak_seg SEGMENT para stack 'STACK'

st_start        db      100h dup(?)     ; �⥪ ����� 0 � RM
Stack_0 = $-st_start

db      100h dup(?)     ; �⥪ ����� 1
Stack_1 = $-st_start

Stak_seg ENDS



; 16-���� ᥣ����, � ���஬ ��室���� ��� ��� �室� � ��室� �� ���饭���� ०���
RM_seg  SEGMENT para public 'CODE' use16

ASSUME cs: RM_seg
ASSUME ds: RM_seg
ASSUME ss: Stak_seg

V86_msg db      "Processor in V86 mode - unable to switch to PM$"
WIN_MSG DB      "Program running under WINDOWS - unable to switch to PL0$"
mes     db      "Return to The Real Mode!!!                                                      $"
Ramk_Col = 03h  ;Color

Header  LABEL   byte
IRPC chr, < --  Task 1  ------------------------------------------------------------------ > 
  db '&chr&', Ramk_Col
ENDM

Footer  LABEL   byte
IRPC chr, < ------------------------------------------------------------------------------ > 
  db '&chr&', Ramk_Col
ENDM

My_msg  LABEL   byte
IRPC chr, <************************ Coded by Max&Roman&Olga IU3-91 ***********************> 
  db '&chr&', 03h
ENDM

Wind PROC near ; �/� ���� ���� �����. �室: di = ��砫쭮� ᬥ饭�� cld
mov     cx, 80
mov     si, offset Header
rep     movsw
mov     al, " "
mov     ah, Ramk_Col
mov     cx, 4
@cycl:  stosw
add     di, 156
stosw
loop    @cycl
mov     cx, 80
mov     si, offset Footer
rep     movsw
ret
Wind    ENDP

TR_text PROC
mov     edi,01E2h
mov     ah,02h
mov     al,20h
mov     cx,78
nxt:
stosw
inc     al
loop    nxt
TR_text ENDP

PicInit PROC near ; �/� ����-�� 1� ���. �室: dl = � Int, ᮮ�-饥 Irq0 
;mov al, 00010101b ; ICW1
mov al, 00010001b ; ICW1
out     20h, al
mov     al, dl
out     21h, al         ; ICW2
mov     al, 00000100b   ; ICW3
out     21h, al
mov     al, 00001101b   ; ICW4
out     21h, al
ret
PicInit ENDP


; -------------------- ��窠 �室� � �ணࠬ�� ------------------------------
Start:
; �����⮢��� ᥣ����� ॣ�����
mov     ax, RM_seg
mov     ds, ax
mov     ax, Stak_seg
mov     ss, ax
mov     sp, Stack_0
mov     ax, 0B800h
mov     es, ax
; �஢����, �� ��室���� �� �� 㦥 � PM
mov     eax, cr0        ; ������ ॣ���� CR0
test    al, 1   ; �஢���� ��� PE, �᫨ �� ���� - �� �����
jz      No_V86  ; �த������, ���� - ᮮ���� �� �訡�� � ���
mov     dx, offset V86_msg
Err_exit:
mov     ah, 9   ; �㭪�� DOS 09h - �뢮� ��ப�
int     21h
mov     ah, 4Ch ; ����� EXE-�ணࠬ��
int     21h
; ����� ����, �� Windows'95 ������ ���, �� PE = 0?
No_V86: mov     ax, 1600h       ; �㭪�� 1600h
int     2Fh             ; ���뢠��� ���⨯�����:
test    al, al          ; �᫨ AL = 0, �
jz      No_win          ; Windows �� ����饭�
mov     dx, offset Win_msg
jmp     short Err_exit  ; ᮮ���� � ���
; �⠪, �� �筮 ��室���� � ॠ�쭮� ०���
No_win:
; ������ ��࠭
mov     ax, 3
int     10h
; ��������� ��࠭
xor     di, di
mov     di, 00A0h
call    Wind            ; ���� ����� A
mov     di, 05A0h
call    Wind            ; ���� ����� B
mov     di, 05B4h
mov     byte ptr es:[di], '2'
mov     di, 0AA0h
call    Wind            ; ���� ����� C
mov     di, 0AB4h
mov     byte ptr es:[di], '3'
mov     cx, 80          ; �뢮� ������ ��ப�
mov     si, offset My_msg
xor     di, di
cld
REP     MOVSW
;       call    TR_text

; ------------ �����⮢�� � ���室� � ���室 � ���饭�� ०�� -------------
; �����⮢��� ॣ���� ds
ASSUME ds: PM_data 
mov ax, PM_data 
mov ds, ax
; ���᫨�� ���� ��� ��� �ᯮ��㥬�� ���ਯ�஢ ᥣ���⮢ 
xor     eax, eax
; 16bitCS                          ; ����� 16bitCS �㤥� ��砫� RM_seg:
mov     ax, RM_seg                 ; AX - ᥣ����� ���� RM_seg
shl     eax, 4                     ; EAX - ������� ���� RM_seg
mov     word ptr GDT_16bitCS+2, ax ; ���� 15 - 0 ����
shr     eax, 16
mov     byte ptr GDT_16bitCS+4, al ; ���� 23 - 16 ����
; 32bitCS
mov     ax, PM_code
shl     eax, 4
mov     word ptr GDT_32bitCS+2, ax ; ����� 32bitCS �㤥� ��砫� PM_code
shr     eax, 16
mov     byte ptr GDT_32bitCS+4, al
; 32bitDS
mov     ax, PM_data
shl     eax, 4
push    eax
mov     word ptr GDT_32bitDS+2, ax ; ����� 32bitDS �㤥� ��砫� PM_data
shr     eax, 16
mov     byte ptr GDT_32bitDS+4, al
; 32bitSS
mov     ax, Stak_seg
shl     eax, 4
mov     word ptr GDT_32bitSS+2, ax ; ����� 32bitSS �㤥� ��砫� Stak_seg
shr     eax, 16
mov     byte ptr GDT_32bitSS+4, al
; ���᫨�� ������� ���� ᥣ���⮢ TSS ���� ����� � �������� �� � ���ਯ���
; TSS ����� 0
pop     eax
push    eax
add     eax, offset TSS_0
mov     word ptr GDT_TSS0+2, ax
shr     eax, 16
mov     byte ptr GDT_TSS0+4, al
; TSS ����� 1
pop     eax
push    eax
add     eax, offset TSS_1
mov     word ptr GDT_TSS1+2, ax
shr     eax, 16
mov     byte ptr GDT_TSS1+4, al
; TSS ����� 2
pop     eax
push    eax
add     eax, offset TSS_2
mov     word ptr GDT_TSS2+2, ax
shr     eax, 16
mov     byte ptr GDT_TSS2+4, al
; CS ����� 2
mov		ax, Task2_CS
mov		es, ax
mov		ax, es:[Task2_CS_Limit]
mov		word ptr ds:[GDT_Task2_CS], ax
mov		eax, 0
mov		ax, Task2_CS
shl		eax, 4
mov		word ptr ds:[GDT_Task2_CS+2], ax
shr		eax, 16
mov		byte ptr ds:[GDT_Task2_CS+4], al
; DS ����� 2
mov		ax, Task2_DS
mov		es, ax
mov		ax, es:[Task2_DS_Limit]
mov		word ptr ds:[GDT_Task2_DS], ax
mov 	eax, 0
mov		ax, Task2_DS
shl		eax, 4
mov		word ptr ds:[GDT_Task2_DS+2], ax
shr		eax, 16
mov		byte ptr ds:[GDT_Task2_DS+4], al
; SS ����� 2 ��� �஢�� �ਢ������ 3
mov 	eax, 0
mov		ax, Task2_SS3
shl		eax, 4
mov		word ptr ds:[GDT_Task2_SS3+2], ax
shr		eax, 16
mov		byte ptr ds:[GDT_Task2_SS3+4], al
; SS ����� 2 ��� �஢�� �ਢ������ 0
mov 	eax, 0
mov		ax, Task2_SS0
shl		eax, 4
mov		word ptr ds:[GDT_Task2_SS0+2], ax
shr		eax, 16
mov		byte ptr ds:[GDT_Task2_SS0+4], al
; LDT ����� 2
mov		ax, ds:[LDT2_Limit]
mov		word ptr ds:[GDT_Task2_LDT], ax
pop     eax
push    eax
add		eax, offset LDT2
mov		word ptr ds:[GDT_Task2_LDT+2], ax
shr		eax, 16
mov		byte ptr ds:[GDT_Task2_LDT+4], al
; ���ਯ�� ᥣ���� ������ ����� 2 � �����쭮� ⠡��� ���ਯ�஢ ����� 2
mov		ax, Task2_DS
mov		es, ax
mov		ax, es:[Task2_DS_Limit]
mov		word ptr ds:[LDT2_DS], ax
mov 	eax, 0
mov		ax, Task2_DS
shl		eax, 4
mov		word ptr ds:[LDT2_DS+2], ax
shr		eax, 16
mov		byte ptr ds:[LDT2_DS+4], al
; ���᫨�� ������� ���� GDT
pop     eax     ; EAX - ������� ���� PM_data
push    eax
add     eax, offset GDT ; EAX - ������� ���� GDT
mov     dword ptr GDTr+2, eax   ; ������� ��� � GDTr
; ���᫨�� ������� ���� IDT
pop     eax     ; EAX - ������� ���� PM_data
add     eax, offset IDT ; EAX - ������� ���� GDT
mov     dword ptr IDTr+2, eax   ; ������� ��� � IDTr
; ����㧨�� GDT
lgdt    fword ptr GDTr
; ����㧨�� IDT
lidt    fword ptr IDTr
; ������� ������� ���뢠���
cli
; � ⠪�� NMI
mov     al, 8Fh         ; ��⠭���� ��� 7 � ��� ����頥� NMI
out     70h, al
jmp     $+2
mov     al,05h
out     71h,al
push    es
mov     ax,40h
mov     es,ax
mov     word ptr es:[67h],offset ret1
mov     word ptr es:[69h],cs
pop     es
; ��२��樠����஢��� ���� ����஫��� ���뢠���,
; � �⮡ࠦ����� Irq0 -> Int 20h ... Irq7 -> Int 27h
mov     dl, 20h
call    PicInit
; �᫨ �� ᮡ�ࠥ��� ࠡ���� � 32-��⭮� �������, �⮨� ������ A20
       mov     al,0D1h
       out     64h,al
       mov     al,0DFh
       out     60h,al
; ��३� � ���饭�� ०��
mov     eax, cr0        ; ������ ॣ���� CR0
or      al, 1   		; ��⠭����� ��� PE � ���
mov     cr0, eax
; ��� ����樨 ���쭥�� jmp � ��⪥ PM_entry:
; ����㧨�� SEL_32bitCS � CS, ᬥ饭�� PM_entry � IP
db      66h     ; ��䨪� ��������� ࠧ�來��� ���࠭��
db      0EAh    ; ��� ������� ���쭥�� jmp
dd      offset PM_entry ; 32-��⭮� ᬥ饭��
dw      SEL_32bitCS     ; ᥫ����


; -------------------- ���४⭮� �����襭�� �ணࠬ�� ----------------------
; ----------------- ��᫥ ������ �� ���饭���� ०��� --------------------
RM_return:      ; � ��।����� �ࠢ����� �� ��室� �� ���饭���� ०���
; ��३� � ॠ��� ०��
;add me
mov     ax,SEL_R_MODE_DATA
mov     ss,ax
mov     ds,ax
mov     es,ax
        db     0EAh
        dw     offset  go
        dw     SEL_16bitCS
go:    mov     eax, cr0        ; ������ ॣ���� CR0
       and     eax, 0FFFFFFFEh ; ����� ��� PE � ���
       mov     cr0, eax
; ����� ��।� �।�롮ન � ����㧨�� CS ॠ��� ᥣ����� ���ᮬ
        db      0EAh   ; ��� ���쭥�� jmp
        dw      $+4    ; ���� ᫥���饩 �������
        dw      RM_seg ; ᥣ����� ���� RM_seg
; ��⠭����� ॣ����� ��� ࠡ���        � ॠ�쭮� ०���
;add me
mov     al,0FEh
out     64h,al
;hlt
ret1:   mov     ax, PM_data
mov     ds, ax
mov     es, ax
mov     ax, Stak_seg
mov     ss, ax
mov     sp, Stack_0
; ����㧨�� IDTR ��� ॠ�쭮�� ०���
       lidt    fword ptr IDTr_real
;A20_OFF
       mov     al,0D1h
       out     64h,al
       mov     al,0DDh
       out     60h,al
; ��२��樠����஢��� ���� ����஫��� ���뢠��� � �⠭���⭮�
; ���ﭨ� (Irq0 -> Int 08h)
       mov     dl, 08h
       call    PicInit
;add me
mov     al,0B8h
out     21h,al
mov     al,9Dh
out     0A1h,al
; ࠧ���� NMI
; ������� ���� CMOS
mov     al, 0   ; ��� ��� 7 �⬥��� �����஢���� NMI
out     70h, al
; ࠧ���� ���뢠���
sti
; � ���
;add me
mov     ax,RM_Seg
mov     ds, ax
mov     es, ax
mov     ah, 09h
mov     dx, offset mes
int     21h
mov     ax, 4C00h
int     21h

RM_seg  ENDS



END Start